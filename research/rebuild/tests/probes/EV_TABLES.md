# Probe EV tables (written by ev_collect.py)

Files scored:

- SCF test records, transformer: transformer | seed 1 (h_scf_test_seed01.parquet, 5 draws); transformer | seed 2 (h_scf_test_seed02.parquet, 5 draws); transformer | seed 3 (h_scf_test_seed03.parquet, 5 draws)
- SCF test records, transformer, bridge hidden: transformer, bridge hidden | seed 1 (h_scf_test_nobr_seed01.parquet, 5 draws); transformer, bridge hidden | seed 2 (h_scf_test_nobr_seed02.parquet, 5 draws); transformer, bridge hidden | seed 3 (h_scf_test_nobr_seed03.parquet, 5 draws)
- SCF test records, transformer, deductions unknown (probe C): transformer, deductions unknown (probe C) | seed 1 (h_scf_test_seed01.parquet, 5 draws); transformer, deductions unknown (probe C) | seed 2 (h_scf_test_seed02.parquet, 5 draws); transformer, deductions unknown (probe C) | seed 3 (h_scf_test_seed03.parquet, 5 draws)
- SCF test records, trees split_leaf20: trees split_leaf20 | file 1 (tr_scf_test_split_leaf20.parquet, 5 draws)
- SCF test records, trees split_leaf50: trees split_leaf50 | file 1 (tr_scf_test_split_leaf50.parquet, 5 draws)
- SCF test records, trees split_leaf50_uf: trees split_leaf50_uf | file 1 (tr_scf_test_split_leaf50_uf.parquet, 5 draws)
- SCF test records, trees split_leaf50_wv: trees split_leaf50_wv | file 1 (tr_scf_test_split_leaf50_wv.parquet, 5 draws)
- SCF test records, trees split_leaf50, bridge hidden: trees split_leaf50, bridge hidden | file 1 (tr_scf_test_nobr_split_leaf50.parquet, 5 draws)
- SCF test records, trees split_leaf50_uf, bridge hidden: trees split_leaf50_uf, bridge hidden | file 1 (tr_scf_test_nobr_split_leaf50_uf.parquet, 5 draws)
- SCF test records, trees split_leaf50_wv, bridge hidden: trees split_leaf50_wv, bridge hidden | file 1 (tr_scf_test_nobr_split_leaf50_wv.parquet, 5 draws)
- PUF records, transformer: transformer | seed 1 (h_puf_wealth_seed01.parquet, 5 draws); transformer | seed 2 (h_puf_wealth_seed02.parquet, 5 draws); transformer | seed 3 (h_puf_wealth_seed03.parquet, 5 draws)
- PUF records, transformer, deductions unknown (probe C): transformer, deductions unknown (probe C) | seed 1 (h_puf_wealth_seed01.parquet, 5 draws); transformer, deductions unknown (probe C) | seed 2 (h_puf_wealth_seed02.parquet, 5 draws); transformer, deductions unknown (probe C) | seed 3 (h_puf_wealth_seed03.parquet, 5 draws)
- PUF records, trees split_leaf20: trees split_leaf20 | file 1 (tr_puf_wealth_split_leaf20.parquet, 5 draws)
- PUF records, trees split_leaf50: trees split_leaf50 | file 1 (tr_puf_wealth_split_leaf50.parquet, 5 draws)
- PUF records, trees split_leaf50_uf: trees split_leaf50_uf | file 1 (tr_puf_wealth_split_leaf50_uf.parquet, 5 draws)
- PUF records, trees split_leaf50_wv: trees split_leaf50_wv | file 1 (tr_puf_wealth_split_leaf50_wv.parquet, 5 draws)
- PUF records, trees zero_leaf50: trees zero_leaf50 | file 1 (tr_puf_wealth_zero_leaf50.parquet, 5 draws)
- Generated records, BL trees, PUF alone (probe BL, minimum leaf 20): bl_sizefirst_leaf20_s1.parquet; bl_sizefirst_leaf20_s2.parquet
- Generated records, Trees joint_leaf20: tr_joint_leaf20_s1.parquet
- Generated records, Trees joint_leaf50: tr_joint_leaf50_s1.parquet; tr_joint_leaf50_s2.parquet
- Generated records, Trees joint_leaf50_uf: tr_joint_leaf50_uf_s1.parquet; tr_joint_leaf50_uf_s2.parquet
- Generated records, Trees joint_leaf50_wv: tr_joint_leaf50_wv_s1.parquet
- Generated records, Transformer (probe C, zero coding): h_cz_random_s1_seed01.parquet; h_cz_random_s1_seed02.parquet; h_cz_random_s1_seed03.parquet
- Generated records, PUF validation: validation

## Summary

Trees column: SCF test records `trees split_leaf50`, PUF records `trees zero_leaf50`, generated records `Trees joint_leaf50`. Transformer: probe C's zero-coded model (`model_cz`), three training seeds. Means over runs, range in brackets.

| Check | Truth | Transformer | Trees | Sampling error | Better |
|---|---|---|---|---|---|
| SCF test records: Per-record score: energy score on ranks of 20 assets and debts (lower is better); random other household: 0.7340 | — | 0.5420 [0.5415–0.5426] | 0.5498 | 0.0014 (of the difference) | transformer |
| SCF test records: Per-record score on the rank of net worth alone (lower is better); random other household: 0.2010 | — | 0.0923 [0.0920–0.0928] | 0.0934 | 0.0009 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Mean net worth by income group: average miss (%); random other household: 150.0 | 0 | 20.7 [17.9–22.6] | 21.0 | 2.7 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Median net worth by income group: average miss (%); random other household: 141.8 | 0 | 13.9 [13.4–14.9] | 18.6 | 5.3 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Holding rates of 9 items by income group: average gap (points); random other household: 22.89 | 0 | 2.57 [2.48–2.71] | 3.06 | 0.26 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Mean net worth by age group: average miss (%); random other household: 206.2 | 0 | 27.0 [26.1–27.7] | 20.4 | 4.0 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Holding rates by age group: average gap (points); random other household: 7.51 | 0 | 1.90 [1.82–2.02] | 1.84 | 0.16 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Mean net worth by filing status: average miss (%); random other household: 92.0 | 0 | 16.0 [13.8–20.0] | 7.6 | 3.9 (of the difference) | trees |
| SCF test records: Holding rates by filing status: average gap (points); random other household: 8.09 | 0 | 1.69 [1.62–1.80] | 1.49 | 0.22 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Untargeted: holding rates of 20 items by income × age, average gap (points); random other household: 8.35 | 0 | 2.39 [2.33–2.43] | 2.36 | 0.07 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Untargeted: mean net worth by income × age, average miss (% of group mean); random other household: 227.5 | 0 | 35.7 [29.7–39.9] | 22.5 | 4.0 (of the difference) | trees |
| SCF test records: Untargeted: joint holding of 190 pairs of items within income groups, average gap (points); random other household: 8.75 | 0 | 2.37 [2.29–2.49] | 3.11 | 0.19 (of the difference) | transformer |
| SCF test records: Untargeted: joint holding of 190 pairs of items, all households, average gap (points); random other household: 0.08 | 0 | 0.55 [0.52–0.59] | 0.50 | 0.06 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Untargeted: rank correlation of net worth with 11 incomes, average gap; random other household: 0.237 | 0 | 0.022 [0.020–0.024] | 0.022 | 0.003 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Untargeted: top 1% share of net worth within income groups, average gap (points); random other household: 22.89 | 0 | 5.58 [5.22–5.80] | 3.41 | 1.27 (of the difference) | neither (within 2 sampling errors) |
| SCF test records: Untargeted: share of homeowners with a mortgage within income groups, average gap (points); random other household: 8.44 | 0 | 0.80 [0.59–1.01] | 1.64 | 0.58 (of the difference) | neither (within 2 sampling errors) |
| SCF test records, mortgage interest and real estate tax hidden: Per-record score: energy score on ranks of 20 assets and debts (lower is better); random other household: 0.7340 | — | 0.5957 [0.5950–0.5963] | 0.6018 | 0.0017 (of the difference) | transformer |
| SCF test records, mortgage interest and real estate tax hidden: Per-record score on the rank of net worth alone (lower is better); random other household: 0.2010 | — | 0.1108 [0.1105–0.1114] | 0.1081 | 0.0010 (of the difference) | trees |
| SCF test records: mean net worth, 99th-99.9th of income | $12.8m | $11.4m [$11.1m–$11.5m] | $11.7m | $1.3m (the truth's) | neither (both within 2 sampling errors of the truth) |
| SCF test records: median net worth, 99th-99.9th of income | $8.0m | $6.1m [$5.9m–$6.3m] | $6.3m | $1.2m (the truth's) | neither (both within 2 sampling errors of the truth) |
| SCF test records: mean net worth, top 0.1% of income | $44.9m | $32.1m [$31.0m–$34.3m] | $19.5m | $9.1m (the truth's) | transformer |
| SCF test records: median net worth, top 0.1% of income | $25.6m | $17.8m [$17.3m–$18.1m] | $12.8m | $8.1m (the truth's) | neither (both within 2 sampling errors of the truth) |
| PUF records: Mean net worth, top 1% of AGI | $16.6m | $13.3m [$12.7m–$13.8m] | $10.2m | $692k (the SCF's) | transformer |
| PUF records: Mean net worth, top 0.1% of AGI | $54.7m | $38.6m [$37.9m–$39.7m] | $25.5m | $3.6m (the SCF's) | transformer |
| PUF records: Median net worth, top 0.1% of AGI | $32.7m | $21.2m [$20.2m–$22.2m] | $13.0m | $3.5m (the SCF's) | transformer |
| PUF records: Mean net worth, 99th–99.9th percentile of AGI | $12.3m | $10.4m [$9.9m–$11.0m] | $8.5m | $704k (the SCF's) | transformer |
| PUF records: Median net worth, all returns | $91k | $24k [$22k–$25k] | $22k | $4k (the SCF's) | about equal (closeness differs by less than 1 sampling error) |
| PUF records: Own a home, all returns (%) | 66.5 | 28.0 [27.7–28.1] | 28.3 | 0.7 (the SCF's) | about equal (closeness differs by less than 1 sampling error) |
| PUF records: Hold a business, top 0.1% of AGI (%) | 85.2 | 73.3 [69.7–76.2] | 68.5 | 6.1 (the SCF's) | about equal (closeness differs by less than 1 sampling error) |
| PUF records: mean net worth by AGI group (5 groups), average miss from the SCF (%) | 0 | 12.3 [10.4–14.2] | 28.6 | — | transformer |
| *PUF records, with non-itemizers' zero mortgage interest and real estate tax not read as 'none paid':* transformer `transformer, deductions unknown (probe C)` against trees `trees split_leaf50` | | | | | |
| PUF records: Mean net worth, top 1% of AGI | $16.6m | $13.0m [$12.3m–$14.1m] | $10.3m | $692k (the SCF's) | transformer |
| PUF records: Mean net worth, top 0.1% of AGI | $54.7m | $37.8m [$34.0m–$44.5m] | $26.2m | $3.6m (the SCF's) | transformer |
| PUF records: Median net worth, top 0.1% of AGI | $32.7m | $20.7m [$19.1m–$22.8m] | $13.3m | $3.5m (the SCF's) | transformer |
| PUF records: Mean net worth, 99th–99.9th percentile of AGI | $12.3m | $10.2m [$9.8m–$10.7m] | $8.6m | $704k (the SCF's) | transformer |
| PUF records: Median net worth, all returns | $91k | $56k [$56k–$57k] | $62k | $4k (the SCF's) | trees |
| PUF records: Own a home, all returns (%) | 66.5 | 56.7 [55.7–58.1] | 60.7 | 0.7 (the SCF's) | trees |
| PUF records: Hold a business, top 0.1% of AGI (%) | 85.2 | 70.8 [66.5–74.0] | 69.2 | 6.1 (the SCF's) | about equal (closeness differs by less than 1 sampling error) |
| PUF records: mean net worth by AGI group (5 groups), average miss from the SCF (%) | 0 | 20.5 [19.6–21.8] | 22.4 | — | transformer |
| Generated records: total AGI ($bn) | 8,094 | 7,819 [7,804–7,845] | 8,184 [8,175–8,193] | 65 (validation file's miss) | trees |
| Generated records: share of AGI above the real 99th percentile (%) | 19.8 | 19.4 [18.5–20.1] | 20.8 [20.7–20.8] | 0.0 (validation file's miss) | about equal (difference within noise) |
| Generated records: net income tax, all returns ($bn; probe V's simplified 2015 tax) | 1,095 | 1,093 [1,077–1,109] | 1,132 [1,130–1,134] | 11 (validation file's miss) | transformer |
| Generated records: net income tax, top 0.1% group ($bn) | 204 | 204 [189–216] | 214 [213–215] | 5 (validation file's miss) | neither (both within noise) |
| Generated records: largest gap in average tax rate by AGI group (points) | 0.00 | 2.69 [2.45–2.91] | 0.19 [0.19–0.20] | 0.15 (validation file) | trees |
| Generated records: sum over 30 amounts × 5 AGI groups of the dollar miss ($bn) | 0 | 877 [860–897] | 368 [353–383] | 492 (validation file) | trees |
| Generated records: share with a nonzero amount, average gap (points) | 0.00 | 2.31 [2.13–2.49] | 0.79 [0.78–0.80] | 0.56 (validation file) | trees |
| Generated records: co-holding of 28 pairs of items, average gap (points) | 0.00 | 2.49 [2.19–2.77] | 0.90 [0.89–0.92] | 0.56 (validation file) | trees |
| Generated records: returns breaking a rule real records never break (%) | 0.00 | 1.00 [0.93–1.07] | 0.30 [0.30–0.30] | 0.02 (validation file) | trees |
| Generated records: classifier telling generated from real, area under the curve (0.5 = cannot tell) | 0.500 | 0.814 [0.812–0.816] | 0.562 [0.561–0.562] | 0.501 (validation file) | trees |
| Generated records: the same within the top 0.1% group | 0.500 | 0.802 [0.797–0.808] | 0.648 [0.646–0.651] | 0.507 (validation file) | trees |
| Generated records: identical to a training record on all 37 variables (%) | — | 25.65 [25.31–26.13] | 28.52 [28.26–28.78] | 26.65 (validation file) | neither (both within 3 points of unseen real records) |

## SCF test records: levels by group

12,021 SCF test records with wealth observed (2,207 household-wave pairs; the five implicates of a household count as one household in the bootstrap). Truth's sampling error: standard deviation over 100 bootstrap resamples of households.

### Income group (seven-income total within the wave)

| Group | Statistic | Truth | Truth's sampling error | transformer | transformer, bridge hidden | transformer, deductions unknown (probe C) | trees split_leaf20 | trees split_leaf50 | trees split_leaf50_uf | trees split_leaf50_wv | trees split_leaf50, bridge hidden | trees split_leaf50_uf, bridge hidden | trees split_leaf50_wv, bridge hidden |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Bottom 50% | mean net worth | $127k | $14k | $158k | $173k | $153k | $148k | $142k | $139k | $147k | $134k | $148k | $142k |
| Bottom 50% | median net worth | $18k | $2k | $17k | $14k | $18k | $17k | $18k | $17k | $18k | $17k | $17k | $19k |
| Bottom 50% | own a home (%) | 47.6 | 1.8 | 47.2 | 44.3 | 47.2 | 48.2 | 48.1 | 48.2 | 48.0 | 48.6 | 49.0 | 48.9 |
| Bottom 50% | have a mortgage (%) | 28.0 | 1.7 | 27.9 | 25.8 | 27.9 | 27.6 | 27.7 | 27.6 | 27.7 | 28.6 | 28.3 | 29.6 |
| Bottom 50% | hold stocks (%) | 8.3 | 1.1 | 8.9 | 8.9 | 9.0 | 8.4 | 8.3 | 8.0 | 8.1 | 8.6 | 7.7 | 7.8 |
| Bottom 50% | hold a business (%) | 5.0 | 0.8 | 5.2 | 5.1 | 5.5 | 6.4 | 6.3 | 6.3 | 6.4 | 6.3 | 6.2 | 6.8 |
| Bottom 50% | hold retirement accounts (%) | 26.1 | 1.7 | 28.8 | 28.1 | 28.6 | 29.1 | 29.0 | 29.7 | 30.2 | 29.2 | 28.8 | 28.7 |
| Bottom 50% | mean home value | $65k | $4k | $68k | $75k | $67k | $73k | $72k | $71k | $70k | $72k | $72k | $74k |
| Bottom 50% | mean stocks | $6k | $2k | $17k | $18k | $17k | $11k | $14k | $11k | $12k | $13k | $12k | $14k |
| Bottom 50% | mean business | $18k | $7k | $26k | $26k | $22k | $20k | $19k | $14k | $16k | $14k | $18k | $16k |
| Bottom 50% | mean retirement accounts | $17k | $3k | $21k | $23k | $21k | $21k | $19k | $19k | $20k | $19k | $18k | $18k |
| 50-90% | mean net worth | $378k | $36k | $514k | $525k | $507k | $473k | $469k | $480k | $479k | $433k | $522k | $476k |
| 50-90% | median net worth | $145k | $10k | $146k | $140k | $145k | $156k | $160k | $157k | $155k | $145k | $146k | $145k |
| 50-90% | own a home (%) | 77.7 | 1.4 | 77.2 | 73.8 | 77.4 | 78.1 | 78.0 | 77.8 | 77.8 | 76.8 | 76.0 | 76.1 |
| 50-90% | have a mortgage (%) | 57.5 | 1.9 | 57.5 | 53.8 | 57.4 | 57.4 | 56.9 | 57.2 | 57.2 | 56.1 | 55.4 | 55.3 |
| 50-90% | hold stocks (%) | 21.9 | 1.5 | 22.3 | 22.4 | 22.5 | 24.0 | 22.8 | 22.9 | 22.9 | 21.8 | 22.4 | 22.9 |
| 50-90% | hold a business (%) | 12.7 | 1.2 | 12.1 | 11.7 | 11.7 | 12.2 | 11.8 | 11.7 | 12.3 | 11.7 | 11.8 | 11.4 |
| 50-90% | hold retirement accounts (%) | 65.8 | 1.8 | 66.1 | 65.2 | 66.4 | 67.0 | 68.2 | 68.5 | 67.1 | 68.2 | 67.7 | 66.8 |
| 50-90% | mean home value | $174k | $6k | $182k | $189k | $181k | $186k | $188k | $188k | $187k | $180k | $177k | $184k |
| 50-90% | mean stocks | $31k | $6k | $62k | $61k | $63k | $57k | $51k | $52k | $49k | $47k | $52k | $52k |
| 50-90% | mean business | $35k | $6k | $93k | $97k | $89k | $64k | $53k | $66k | $69k | $55k | $120k | $66k |
| 50-90% | mean retirement accounts | $90k | $7k | $97k | $101k | $97k | $96k | $96k | $96k | $95k | $90k | $97k | $95k |
| 90-99% | mean net worth | $1.9m | $143k | $2.0m | $2.1m | $1.9m | $2.3m | $2.0m | $2.1m | $2.2m | $2.0m | $2.0m | $2.2m |
| 90-99% | median net worth | $732k | $96k | $799k | $809k | $785k | $862k | $808k | $866k | $898k | $862k | $891k | $909k |
| 90-99% | own a home (%) | 92.6 | 1.6 | 92.1 | 91.2 | 92.1 | 92.9 | 93.0 | 92.6 | 92.8 | 90.7 | 90.2 | 89.8 |
| 90-99% | have a mortgage (%) | 77.8 | 2.5 | 78.5 | 72.2 | 78.3 | 78.5 | 79.0 | 78.3 | 78.2 | 72.3 | 70.3 | 68.9 |
| 90-99% | hold stocks (%) | 49.8 | 3.0 | 48.6 | 48.8 | 48.5 | 51.6 | 50.0 | 51.7 | 51.0 | 50.2 | 51.4 | 51.7 |
| 90-99% | hold a business (%) | 28.5 | 2.7 | 25.1 | 24.9 | 24.4 | 29.5 | 25.5 | 27.7 | 27.0 | 27.3 | 27.0 | 28.0 |
| 90-99% | hold retirement accounts (%) | 88.5 | 2.1 | 88.8 | 88.8 | 89.2 | 89.9 | 90.8 | 90.6 | 90.7 | 89.8 | 91.2 | 91.2 |
| 90-99% | mean home value | $497k | $23k | $516k | $533k | $508k | $518k | $505k | $505k | $505k | $491k | $530k | $497k |
| 90-99% | mean stocks | $278k | $43k | $294k | $308k | $296k | $324k | $295k | $246k | $267k | $236k | $253k | $248k |
| 90-99% | mean business | $434k | $65k | $443k | $541k | $421k | $659k | $451k | $589k | $589k | $512k | $487k | $589k |
| 90-99% | mean retirement accounts | $385k | $34k | $368k | $372k | $370k | $340k | $364k | $368k | $398k | $386k | $374k | $399k |
| 99-99.9% | mean net worth | $12.8m | $1.3m | $11.4m | $11.2m | $10.9m | $13.0m | $11.7m | $11.4m | $11.7m | $10.4m | $12.2m | $11.0m |
| 99-99.9% | median net worth | $8.0m | $1.2m | $6.1m | $6.2m | $5.8m | $6.8m | $6.3m | $6.3m | $6.0m | $5.7m | $6.9m | $5.8m |
| 99-99.9% | own a home (%) | 94.4 | 4.9 | 93.1 | 95.0 | 92.9 | 93.7 | 93.4 | 93.3 | 93.5 | 97.4 | 96.6 | 97.6 |
| 99-99.9% | have a mortgage (%) | 63.4 | 6.7 | 62.2 | 62.5 | 61.5 | 62.1 | 62.5 | 63.0 | 63.4 | 61.5 | 59.5 | 59.0 |
| 99-99.9% | hold stocks (%) | 80.4 | 5.5 | 78.6 | 79.3 | 78.4 | 75.8 | 77.1 | 80.2 | 78.7 | 77.2 | 79.2 | 79.7 |
| 99-99.9% | hold a business (%) | 63.2 | 7.1 | 54.6 | 53.6 | 55.1 | 63.1 | 65.3 | 61.3 | 54.5 | 61.5 | 58.1 | 56.8 |
| 99-99.9% | hold retirement accounts (%) | 87.5 | 5.3 | 90.6 | 89.6 | 90.5 | 94.1 | 93.3 | 87.9 | 88.4 | 93.1 | 88.1 | 89.8 |
| 99-99.9% | mean home value | $1.9m | $489k | $1.4m | $1.4m | $1.3m | $1.4m | $1.4m | $1.4m | $1.4m | $1.2m | $1.4m | $1.3m |
| 99-99.9% | mean stocks | $2.4m | $356k | $2.0m | $2.1m | $2.0m | $2.1m | $2.1m | $1.9m | $2.1m | $1.9m | $2.3m | $1.9m |
| 99-99.9% | mean business | $3.0m | $506k | $4.1m | $3.7m | $3.7m | $5.2m | $4.6m | $4.2m | $4.3m | $3.3m | $4.6m | $4.2m |
| 99-99.9% | mean retirement accounts | $1.2m | $148k | $1.1m | $1.1m | $1.0m | $1.2m | $942k | $913k | $947k | $1.2m | $922k | $981k |
| Top 0.1% | mean net worth | $44.9m | $9.1m | $32.1m | $30.9m | $31.0m | $25.7m | $19.5m | $33.8m | $33.0m | $20.5m | $34.1m | $37.2m |
| Top 0.1% | median net worth | $25.6m | $8.1m | $17.8m | $16.7m | $17.5m | $13.5m | $12.8m | $17.1m | $16.1m | $11.4m | $16.5m | $12.3m |
| Top 0.1% | own a home (%) | 99.7 | 0.2 | 99.6 | 97.6 | 99.5 | 99.9 | 99.7 | 99.7 | 99.7 | 96.5 | 99.5 | 99.4 |
| Top 0.1% | have a mortgage (%) | 63.7 | 10.2 | 62.5 | 63.2 | 62.0 | 61.0 | 60.1 | 61.1 | 58.5 | 54.2 | 54.4 | 44.3 |
| Top 0.1% | hold stocks (%) | 96.4 | 1.7 | 94.4 | 94.7 | 93.3 | 78.1 | 82.1 | 94.1 | 94.3 | 87.8 | 93.4 | 92.3 |
| Top 0.1% | hold a business (%) | 84.0 | 7.6 | 67.1 | 66.6 | 69.0 | 67.8 | 63.6 | 69.4 | 66.5 | 69.1 | 69.3 | 60.6 |
| Top 0.1% | hold retirement accounts (%) | 93.7 | 2.6 | 94.6 | 93.1 | 93.8 | 91.8 | 92.0 | 93.3 | 90.8 | 93.1 | 90.8 | 89.6 |
| Top 0.1% | mean home value | $2.8m | $461k | $2.1m | $2.2m | $2.2m | $2.5m | $2.4m | $2.5m | $2.5m | $2.1m | $2.2m | $2.6m |
| Top 0.1% | mean stocks | $8.6m | $2.0m | $6.7m | $6.4m | $7.0m | $6.1m | $4.6m | $7.9m | $7.7m | $4.2m | $7.1m | $7.2m |
| Top 0.1% | mean business | $17.6m | $4.9m | $12.9m | $12.4m | $12.0m | $7.8m | $5.9m | $14.0m | $13.4m | $7.7m | $15.0m | $14.5m |
| Top 0.1% | mean retirement accounts | $2.4m | $565k | $1.7m | $1.7m | $1.7m | $1.2m | $1.1m | $1.3m | $1.1m | $1.2m | $1.4m | $997k |

### Age of the head

| Group | Statistic | Truth | Truth's sampling error | transformer | transformer, bridge hidden | transformer, deductions unknown (probe C) | trees split_leaf20 | trees split_leaf50 | trees split_leaf50_uf | trees split_leaf50_wv | trees split_leaf50, bridge hidden | trees split_leaf50_uf, bridge hidden | trees split_leaf50_wv, bridge hidden |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| under 35 | mean net worth | $63k | $9k | $100k | $97k | $94k | $99k | $93k | $98k | $95k | $84k | $97k | $95k |
| under 35 | median net worth | $11k | $2k | $11k | $9k | $11k | $11k | $11k | $11k | $12k | $10k | $10k | $12k |
| under 35 | own a home (%) | 36.2 | 2.7 | 36.5 | 33.5 | 36.6 | 36.8 | 37.0 | 36.8 | 37.0 | 37.2 | 37.5 | 38.4 |
| under 35 | have a mortgage (%) | 30.1 | 2.7 | 29.6 | 28.4 | 29.6 | 29.3 | 29.1 | 29.2 | 29.5 | 30.2 | 31.2 | 31.6 |
| under 35 | hold stocks (%) | 13.6 | 1.8 | 11.4 | 11.3 | 11.5 | 11.4 | 11.1 | 10.9 | 10.3 | 10.8 | 10.6 | 10.8 |
| under 35 | hold a business (%) | 4.5 | 1.1 | 5.5 | 5.5 | 5.5 | 6.5 | 6.4 | 6.5 | 6.3 | 5.7 | 6.1 | 6.2 |
| under 35 | hold retirement accounts (%) | 38.2 | 2.6 | 40.5 | 39.5 | 40.8 | 40.2 | 41.1 | 41.9 | 41.5 | 41.6 | 41.7 | 40.6 |
| under 35 | mean home value | $52k | $5k | $57k | $61k | $58k | $57k | $60k | $62k | $59k | $56k | $58k | $61k |
| under 35 | mean stocks | $8k | $4k | $10k | $11k | $10k | $9k | $10k | $11k | $13k | $10k | $13k | $11k |
| under 35 | mean business | $10k | $5k | $27k | $19k | $22k | $22k | $17k | $16k | $12k | $15k | $18k | $14k |
| under 35 | mean retirement accounts | $12k | $2k | $19k | $20k | $18k | $13k | $15k | $15k | $18k | $14k | $18k | $18k |
| 35-44 | mean net worth | $253k | $31k | $402k | $403k | $404k | $398k | $366k | $364k | $377k | $353k | $377k | $353k |
| 35-44 | median net worth | $46k | $7k | $53k | $47k | $53k | $51k | $55k | $52k | $57k | $52k | $50k | $53k |
| 35-44 | own a home (%) | 61.4 | 2.4 | 60.5 | 58.4 | 60.8 | 60.9 | 61.1 | 60.7 | 60.5 | 63.4 | 60.7 | 61.0 |
| 35-44 | have a mortgage (%) | 54.7 | 2.7 | 54.3 | 51.2 | 54.4 | 53.7 | 53.8 | 53.9 | 53.9 | 54.1 | 51.2 | 51.4 |
| 35-44 | hold stocks (%) | 15.0 | 1.9 | 14.0 | 14.1 | 14.0 | 15.9 | 15.3 | 15.5 | 14.7 | 15.0 | 14.7 | 15.0 |
| 35-44 | hold a business (%) | 14.8 | 1.7 | 11.4 | 11.3 | 11.0 | 12.6 | 11.5 | 11.5 | 12.0 | 11.9 | 11.9 | 11.5 |
| 35-44 | hold retirement accounts (%) | 55.9 | 2.8 | 54.9 | 54.2 | 54.8 | 53.5 | 53.8 | 53.7 | 54.5 | 55.2 | 53.9 | 54.1 |
| 35-44 | mean home value | $149k | $11k | $167k | $175k | $168k | $168k | $165k | $164k | $168k | $168k | $165k | $162k |
| 35-44 | mean stocks | $24k | $7k | $44k | $49k | $44k | $44k | $52k | $34k | $35k | $29k | $35k | $30k |
| 35-44 | mean business | $65k | $19k | $120k | $105k | $117k | $120k | $85k | $98k | $113k | $109k | $116k | $90k |
| 35-44 | mean retirement accounts | $44k | $5k | $78k | $80k | $78k | $56k | $55k | $58k | $63k | $58k | $56k | $67k |
| 45-54 | mean net worth | $598k | $62k | $653k | $706k | $626k | $734k | $625k | $693k | $709k | $632k | $706k | $796k |
| 45-54 | median net worth | $108k | $20k | $123k | $111k | $123k | $136k | $139k | $140k | $142k | $126k | $131k | $133k |
| 45-54 | own a home (%) | 75.4 | 2.2 | 75.0 | 68.3 | 74.9 | 75.4 | 75.4 | 75.3 | 75.1 | 72.1 | 72.9 | 71.9 |
| 45-54 | have a mortgage (%) | 61.9 | 2.5 | 62.9 | 57.1 | 62.7 | 62.7 | 62.4 | 62.7 | 62.3 | 59.4 | 57.7 | 58.1 |
| 45-54 | hold stocks (%) | 19.9 | 2.3 | 18.8 | 18.9 | 19.0 | 21.9 | 20.5 | 19.7 | 21.0 | 19.1 | 19.8 | 20.5 |
| 45-54 | hold a business (%) | 15.7 | 1.6 | 14.3 | 13.8 | 13.8 | 14.0 | 12.7 | 14.2 | 14.2 | 14.4 | 13.7 | 14.5 |
| 45-54 | hold retirement accounts (%) | 59.2 | 2.8 | 62.4 | 61.5 | 62.1 | 64.0 | 64.7 | 63.5 | 63.2 | 62.2 | 63.0 | 62.6 |
| 45-54 | mean home value | $228k | $26k | $227k | $227k | $224k | $219k | $222k | $218k | $222k | $214k | $216k | $223k |
| 45-54 | mean stocks | $63k | $14k | $70k | $84k | $76k | $86k | $63k | $73k | $74k | $55k | $79k | $75k |
| 45-54 | mean business | $135k | $21k | $189k | $212k | $164k | $235k | $160k | $213k | $199k | $191k | $205k | $295k |
| 45-54 | mean retirement accounts | $113k | $13k | $122k | $125k | $124k | $119k | $137k | $135k | $154k | $132k | $137k | $143k |
| 55-64 | mean net worth | $1.2m | $108k | $1.3m | $1.3m | $1.3m | $1.2m | $1.2m | $1.3m | $1.3m | $1.2m | $1.3m | $1.3m |
| 55-64 | median net worth | $249k | $26k | $238k | $225k | $239k | $253k | $241k | $261k | $247k | $230k | $238k | $238k |
| 55-64 | own a home (%) | 82.7 | 1.9 | 82.8 | 76.4 | 82.9 | 83.8 | 83.2 | 83.5 | 83.3 | 81.3 | 81.2 | 79.5 |
| 55-64 | have a mortgage (%) | 54.9 | 2.7 | 55.5 | 53.2 | 55.2 | 55.6 | 55.2 | 54.7 | 55.0 | 56.0 | 56.0 | 54.7 |
| 55-64 | hold stocks (%) | 24.9 | 2.4 | 28.2 | 28.4 | 28.2 | 28.9 | 27.5 | 27.9 | 27.7 | 27.3 | 28.3 | 28.1 |
| 55-64 | hold a business (%) | 15.6 | 1.9 | 17.1 | 16.6 | 17.3 | 18.3 | 18.2 | 17.8 | 18.1 | 18.1 | 17.6 | 17.7 |
| 55-64 | hold retirement accounts (%) | 66.2 | 2.9 | 63.1 | 62.2 | 63.0 | 64.4 | 63.7 | 64.9 | 63.0 | 64.5 | 63.5 | 62.8 |
| 55-64 | mean home value | $276k | $17k | $281k | $285k | $277k | $290k | $277k | $284k | $286k | $267k | $281k | $272k |
| 55-64 | mean stocks | $167k | $28k | $208k | $198k | $209k | $187k | $165k | $173k | $185k | $143k | $175k | $179k |
| 55-64 | mean business | $263k | $27k | $330k | $354k | $305k | $266k | $263k | $370k | $342k | $290k | $340k | $328k |
| 55-64 | mean retirement accounts | $216k | $22k | $201k | $202k | $202k | $200k | $197k | $198k | $190k | $216k | $190k | $184k |
| 65-74 | mean net worth | $1.3m | $158k | $1.2m | $1.3m | $1.2m | $1.4m | $1.2m | $1.2m | $1.2m | $1.1m | $1.3m | $1.2m |
| 65-74 | median net worth | $229k | $29k | $254k | $295k | $252k | $273k | $271k | $276k | $268k | $289k | $295k | $290k |
| 65-74 | own a home (%) | 86.8 | 2.5 | 85.4 | 87.3 | 85.5 | 87.5 | 87.5 | 87.5 | 87.1 | 86.1 | 85.8 | 87.6 |
| 65-74 | have a mortgage (%) | 52.6 | 4.0 | 52.3 | 38.0 | 52.2 | 52.1 | 52.1 | 52.3 | 52.4 | 38.4 | 38.7 | 39.9 |
| 65-74 | hold stocks (%) | 23.7 | 3.0 | 29.0 | 29.3 | 29.2 | 27.4 | 27.0 | 29.4 | 28.9 | 28.3 | 27.2 | 27.8 |
| 65-74 | hold a business (%) | 13.2 | 2.1 | 11.4 | 11.2 | 11.6 | 15.0 | 13.7 | 13.2 | 13.7 | 12.9 | 13.7 | 13.4 |
| 65-74 | hold retirement accounts (%) | 47.1 | 3.8 | 49.3 | 49.1 | 49.8 | 50.7 | 52.0 | 54.5 | 52.1 | 54.5 | 52.7 | 52.5 |
| 65-74 | mean home value | $268k | $26k | $249k | $287k | $243k | $270k | $274k | $272k | $259k | $260k | $261k | $275k |
| 65-74 | mean stocks | $165k | $29k | $193k | $196k | $190k | $179k | $169k | $167k | $170k | $195k | $177k | $162k |
| 65-74 | mean business | $246k | $50k | $283k | $282k | $272k | $422k | $289k | $258k | $253k | $145k | $280k | $212k |
| 65-74 | mean retirement accounts | $202k | $30k | $163k | $175k | $160k | $182k | $145k | $158k | $159k | $168k | $174k | $170k |
| 75 and over | mean net worth | $1.5m | $177k | $1.2m | $1.2m | $1.2m | $1.4m | $1.3m | $1.2m | $1.3m | $1.1m | $1.4m | $1.2m |
| 75 and over | median net worth | $307k | $101k | $324k | $346k | $311k | $346k | $319k | $326k | $319k | $328k | $330k | $314k |
| 75 and over | own a home (%) | 84.0 | 3.5 | 82.8 | 89.0 | 82.9 | 86.1 | 85.8 | 85.6 | 85.8 | 87.4 | 87.9 | 88.0 |
| 75 and over | have a mortgage (%) | 21.1 | 3.5 | 20.6 | 29.7 | 20.6 | 21.4 | 21.6 | 21.4 | 21.6 | 32.8 | 31.8 | 34.5 |
| 75 and over | hold stocks (%) | 35.0 | 4.4 | 36.2 | 35.5 | 36.3 | 34.7 | 34.5 | 35.0 | 35.3 | 35.0 | 34.2 | 34.8 |
| 75 and over | hold a business (%) | 8.0 | 2.0 | 7.0 | 6.8 | 7.0 | 8.9 | 8.4 | 8.5 | 8.5 | 8.8 | 8.0 | 8.7 |
| 75 and over | hold retirement accounts (%) | 38.4 | 4.7 | 46.0 | 46.4 | 46.7 | 49.6 | 51.1 | 50.4 | 53.5 | 47.9 | 49.1 | 49.7 |
| 75 and over | mean home value | $277k | $27k | $238k | $261k | $228k | $272k | $269k | $261k | $265k | $245k | $264k | $244k |
| 75 and over | mean stocks | $266k | $53k | $226k | $223k | $227k | $246k | $252k | $212k | $206k | $182k | $221k | $214k |
| 75 and over | mean business | $246k | $77k | $160k | $203k | $170k | $161k | $163k | $162k | $270k | $104k | $411k | $152k |
| 75 and over | mean retirement accounts | $174k | $33k | $140k | $141k | $134k | $149k | $151k | $132k | $127k | $123k | $128k | $135k |

### Filing status

| Group | Statistic | Truth | Truth's sampling error | transformer | transformer, bridge hidden | transformer, deductions unknown (probe C) | trees split_leaf20 | trees split_leaf50 | trees split_leaf50_uf | trees split_leaf50_wv | trees split_leaf50, bridge hidden | trees split_leaf50_uf, bridge hidden | trees split_leaf50_wv, bridge hidden |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| single | mean net worth | $355k | $37k | $383k | $394k | $369k | $413k | $374k | $368k | $412k | $345k | $370k | $379k |
| single | median net worth | $48k | $7k | $57k | $51k | $56k | $63k | $59k | $64k | $64k | $58k | $57k | $60k |
| single | own a home (%) | 57.4 | 1.9 | 56.9 | 53.9 | 56.9 | 58.6 | 58.4 | 58.5 | 58.2 | 58.2 | 58.3 | 58.4 |
| single | have a mortgage (%) | 34.9 | 1.9 | 34.2 | 30.1 | 34.2 | 33.9 | 33.7 | 33.7 | 33.8 | 34.2 | 33.9 | 35.0 |
| single | hold stocks (%) | 17.3 | 1.7 | 18.2 | 18.0 | 18.1 | 17.9 | 18.0 | 17.6 | 17.5 | 17.6 | 16.6 | 16.8 |
| single | hold a business (%) | 7.9 | 1.2 | 6.5 | 6.2 | 6.7 | 7.8 | 7.2 | 7.3 | 7.6 | 7.3 | 7.1 | 7.9 |
| single | hold retirement accounts (%) | 41.8 | 2.1 | 44.4 | 43.6 | 44.2 | 45.3 | 45.7 | 46.9 | 46.3 | 45.3 | 45.4 | 44.9 |
| single | mean home value | $117k | $8k | $108k | $114k | $107k | $122k | $117k | $118k | $115k | $113k | $115k | $113k |
| single | mean stocks | $46k | $11k | $67k | $68k | $68k | $63k | $64k | $59k | $63k | $55k | $64k | $62k |
| single | mean business | $52k | $12k | $63k | $62k | $54k | $54k | $41k | $48k | $70k | $35k | $48k | $53k |
| single | mean retirement accounts | $51k | $5k | $52k | $51k | $51k | $60k | $60k | $53k | $59k | $55k | $55k | $56k |
| married filing jointly | mean net worth | $1.1m | $61k | $1.2m | $1.2m | $1.2m | $1.2m | $1.1m | $1.2m | $1.2m | $1.0m | $1.2m | $1.2m |
| married filing jointly | median net worth | $208k | $12k | $244k | $246k | $245k | $236k | $237k | $240k | $239k | $227k | $236k | $234k |
| married filing jointly | own a home (%) | 83.0 | 1.1 | 82.7 | 82.5 | 82.8 | 83.2 | 83.2 | 83.0 | 83.3 | 81.8 | 80.8 | 81.3 |
| married filing jointly | have a mortgage (%) | 62.0 | 1.5 | 62.5 | 61.6 | 62.3 | 62.6 | 62.4 | 62.4 | 62.5 | 60.5 | 58.9 | 59.5 |
| married filing jointly | hold stocks (%) | 26.5 | 1.4 | 27.9 | 28.0 | 28.0 | 28.9 | 27.5 | 28.7 | 28.5 | 27.5 | 28.3 | 28.6 |
| married filing jointly | hold a business (%) | 16.3 | 1.1 | 16.7 | 16.5 | 16.3 | 17.5 | 16.7 | 16.8 | 16.8 | 16.8 | 16.9 | 16.6 |
| married filing jointly | hold retirement accounts (%) | 65.8 | 1.7 | 67.3 | 67.2 | 67.7 | 67.6 | 68.5 | 68.8 | 68.1 | 68.5 | 68.3 | 67.8 |
| married filing jointly | mean home value | $292k | $14k | $300k | $318k | $295k | $298k | $299k | $299k | $301k | $287k | $294k | $296k |
| married filing jointly | mean stocks | $149k | $15k | $163k | $167k | $166k | $165k | $146k | $145k | $148k | $128k | $150k | $145k |
| married filing jointly | mean business | $232k | $22k | $303k | $324k | $288k | $340k | $267k | $322k | $306k | $251k | $362k | $317k |
| married filing jointly | mean retirement accounts | $187k | $13k | $189k | $195k | $190k | $173k | $171k | $178k | $181k | $183k | $177k | $182k |
| married filing separately | mean net worth | $704k | $204k | $516k | $575k | $482k | $588k | $583k | $579k | $743k | $594k | $802k | $771k |
| married filing separately | median net worth | $20k | $20k | $39k | $41k | $39k | $37k | $44k | $38k | $37k | $52k | $51k | $52k |
| married filing separately | own a home (%) | 54.4 | 8.1 | 54.4 | 55.7 | 54.6 | 55.0 | 54.8 | 55.1 | 55.1 | 60.3 | 65.6 | 60.5 |
| married filing separately | have a mortgage (%) | 31.9 | 8.7 | 32.8 | 40.8 | 32.6 | 32.4 | 33.3 | 33.9 | 33.1 | 44.0 | 49.4 | 42.9 |
| married filing separately | hold stocks (%) | 18.6 | 7.2 | 19.8 | 20.0 | 19.2 | 20.1 | 19.4 | 18.8 | 19.7 | 20.5 | 20.3 | 21.9 |
| married filing separately | hold a business (%) | 15.0 | 4.9 | 12.7 | 13.1 | 13.2 | 17.5 | 15.7 | 18.8 | 17.6 | 13.0 | 16.6 | 17.0 |
| married filing separately | hold retirement accounts (%) | 45.6 | 7.8 | 48.0 | 49.5 | 48.1 | 46.0 | 45.6 | 46.5 | 47.4 | 48.5 | 48.2 | 46.5 |
| married filing separately | mean home value | $122k | $44k | $103k | $122k | $99k | $106k | $104k | $105k | $116k | $125k | $165k | $140k |
| married filing separately | mean stocks | $100k | $31k | $87k | $102k | $89k | $87k | $84k | $80k | $86k | $98k | $95k | $75k |
| married filing separately | mean business | $218k | $113k | $178k | $168k | $145k | $189k | $194k | $191k | $253k | $190k | $289k | $288k |
| married filing separately | mean retirement accounts | $75k | $22k | $59k | $69k | $55k | $77k | $74k | $74k | $88k | $61k | $89k | $87k |
| head of household | mean net worth | $173k | $33k | $138k | $137k | $131k | $143k | $159k | $145k | $142k | $147k | $145k | $131k |
| head of household | median net worth | $16k | $3k | $10k | $8k | $10k | $11k | $12k | $11k | $12k | $11k | $11k | $12k |
| head of household | own a home (%) | 44.5 | 3.2 | 43.8 | 33.5 | 44.0 | 44.1 | 44.2 | 43.9 | 43.5 | 44.2 | 44.4 | 43.7 |
| head of household | have a mortgage (%) | 35.8 | 3.3 | 36.0 | 26.2 | 36.0 | 35.2 | 35.4 | 35.4 | 35.2 | 33.4 | 33.3 | 32.6 |
| head of household | hold stocks (%) | 8.9 | 2.0 | 5.3 | 5.4 | 5.6 | 7.9 | 6.7 | 5.9 | 5.9 | 6.1 | 6.1 | 6.5 |
| head of household | hold a business (%) | 8.0 | 1.5 | 6.0 | 5.7 | 5.8 | 7.6 | 7.0 | 7.4 | 7.9 | 7.9 | 6.9 | 7.0 |
| head of household | hold retirement accounts (%) | 35.3 | 3.0 | 33.8 | 31.3 | 33.5 | 35.2 | 35.5 | 34.4 | 35.1 | 36.0 | 34.6 | 34.8 |
| head of household | mean home value | $68k | $7k | $68k | $58k | $68k | $74k | $76k | $72k | $70k | $69k | $71k | $68k |
| head of household | mean stocks | $20k | $9k | $11k | $13k | $11k | $13k | $17k | $14k | $11k | $10k | $13k | $11k |
| head of household | mean business | $44k | $13k | $37k | $30k | $32k | $30k | $27k | $30k | $32k | $46k | $30k | $29k |
| head of household | mean retirement accounts | $28k | $7k | $23k | $23k | $22k | $24k | $28k | $25k | $26k | $26k | $28k | $24k |

## SCF test records: relationships no target covers

Rank correlation of net worth with each income:

| | Truth | Truth's sampling error | transformer | transformer, bridge hidden | transformer, deductions unknown (probe C) | trees split_leaf20 | trees split_leaf50 | trees split_leaf50_uf | trees split_leaf50_wv | trees split_leaf50, bridge hidden | trees split_leaf50_uf, bridge hidden | trees split_leaf50_wv, bridge hidden |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| wages | 0.236 | 0.024 | 0.220 | 0.204 | 0.219 | 0.224 | 0.232 | 0.227 | 0.231 | 0.231 | 0.226 | 0.230 |
| Schedule C | 0.078 | 0.024 | 0.058 | 0.046 | 0.059 | 0.064 | 0.063 | 0.059 | 0.060 | 0.067 | 0.063 | 0.062 |
| Schedule E | 0.145 | 0.024 | 0.121 | 0.116 | 0.119 | 0.117 | 0.116 | 0.113 | 0.110 | 0.122 | 0.113 | 0.111 |
| long-term gains | 0.227 | 0.026 | 0.211 | 0.211 | 0.210 | 0.213 | 0.211 | 0.202 | 0.203 | 0.216 | 0.208 | 0.210 |
| dividends | 0.442 | 0.021 | 0.437 | 0.443 | 0.433 | 0.430 | 0.426 | 0.427 | 0.424 | 0.434 | 0.427 | 0.432 |
| taxable interest | 0.582 | 0.016 | 0.562 | 0.566 | 0.560 | 0.566 | 0.559 | 0.561 | 0.558 | 0.564 | 0.558 | 0.563 |
| taxable pensions | 0.266 | 0.023 | 0.244 | 0.265 | 0.244 | 0.251 | 0.243 | 0.243 | 0.236 | 0.263 | 0.252 | 0.251 |
| Social Security | 0.235 | 0.022 | 0.210 | 0.238 | 0.209 | 0.215 | 0.208 | 0.209 | 0.202 | 0.234 | 0.224 | 0.222 |
| farm | 0.085 | 0.024 | 0.054 | 0.053 | 0.055 | 0.051 | 0.051 | 0.050 | 0.046 | 0.064 | 0.051 | 0.053 |
| tax-exempt interest | 0.225 | 0.024 | 0.189 | 0.188 | 0.189 | 0.190 | 0.191 | 0.186 | 0.184 | 0.201 | 0.190 | 0.192 |
| unemployment | 0.069 | 0.024 | 0.044 | 0.042 | 0.044 | 0.044 | 0.047 | 0.045 | 0.042 | 0.065 | 0.050 | 0.053 |

Top 1% share of net worth within each income group (%):

| | Truth | Truth's sampling error | transformer | transformer, bridge hidden | transformer, deductions unknown (probe C) | trees split_leaf20 | trees split_leaf50 | trees split_leaf50_uf | trees split_leaf50_wv | trees split_leaf50, bridge hidden | trees split_leaf50_uf, bridge hidden | trees split_leaf50_wv, bridge hidden |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Bottom 50% | 31.3 | 5.0 | 37.1 | 36.6 | 35.7 | 30.4 | 30.9 | 26.8 | 29.2 | 27.4 | 31.2 | 28.4 |
| 50-90% | 20.0 | 4.4 | 28.4 | 28.0 | 28.9 | 21.6 | 21.7 | 23.1 | 23.2 | 21.6 | 30.9 | 23.2 |
| 90-99% | 11.8 | 1.1 | 18.5 | 21.6 | 18.1 | 23.8 | 19.1 | 17.9 | 21.1 | 19.9 | 15.6 | 20.3 |
| 99-99.9% | 10.5 | 1.7 | 14.4 | 12.3 | 14.9 | 15.0 | 15.9 | 15.3 | 17.5 | 12.8 | 16.0 | 16.4 |
| Top 0.1% | 13.5 | 2.1 | 10.3 | 11.8 | 12.0 | 15.7 | 11.2 | 16.9 | 12.7 | 12.6 | 17.1 | 13.8 |

Holding one item given another (%):

| | Truth | Truth's sampling error | transformer | transformer, bridge hidden | transformer, deductions unknown (probe C) | trees split_leaf20 | trees split_leaf50 | trees split_leaf50_uf | trees split_leaf50_wv | trees split_leaf50, bridge hidden | trees split_leaf50_uf, bridge hidden | trees split_leaf50_wv, bridge hidden |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| mortgage among homeowners | 70.9 | 1.6 | 71.3 | 69.4 | 71.1 | 70.0 | 69.5 | 69.6 | 69.7 | 69.4 | 68.6 | 69.1 |
| stocks among business owners | 36.9 | 3.0 | 40.0 | 40.4 | 41.0 | 41.2 | 38.8 | 40.2 | 40.9 | 39.3 | 39.4 | 40.8 |
| business gains among business owners | 90.8 | 2.0 | 94.3 | 94.2 | 93.2 | 90.5 | 90.1 | 90.7 | 89.9 | 89.9 | 90.0 | 90.3 |
| mortgage among owners of other homes | 41.8 | 3.1 | 38.9 | 43.3 | 39.0 | 37.6 | 38.7 | 38.8 | 39.3 | 39.7 | 39.4 | 39.8 |
| mortgage among homeowners, Bottom 50% | 58.9 | 2.9 | 59.1 | 58.2 | 58.9 | 57.0 | 56.8 | 56.5 | 56.9 | 58.8 | 57.8 | 60.3 |
| mortgage among homeowners, 50-90% | 74.0 | 2.1 | 74.3 | 72.8 | 74.2 | 73.5 | 72.5 | 72.9 | 73.0 | 73.1 | 72.8 | 72.6 |
| mortgage among homeowners, 90-99% | 84.0 | 2.2 | 85.3 | 79.2 | 85.0 | 84.3 | 84.6 | 84.4 | 84.1 | 79.6 | 78.0 | 76.6 |
| mortgage among homeowners, 99-99.9% | 67.2 | 5.7 | 66.8 | 65.8 | 66.1 | 66.3 | 66.9 | 67.6 | 67.8 | 63.0 | 61.6 | 60.4 |
| mortgage among homeowners, Top 0.1% | 63.9 | 10.2 | 62.7 | 64.7 | 62.3 | 61.1 | 60.2 | 61.3 | 58.6 | 55.4 | 54.6 | 44.6 |

Per-record score by income group (energy score, lower is better):

| | transformer | transformer, bridge hidden | transformer, deductions unknown (probe C) | trees split_leaf20 | trees split_leaf50 | trees split_leaf50_uf | trees split_leaf50_wv | trees split_leaf50, bridge hidden | trees split_leaf50_uf, bridge hidden | trees split_leaf50_wv, bridge hidden |
|---|---|---|---|---|---|---|---|---|---|---|
| Bottom 50% | 0.4563 | 0.5038 | 0.4581 | 0.4687 | 0.4579 | 0.4635 | 0.4609 | 0.5036 | 0.5030 | 0.5034 |
| 50-90% | 0.5888 | 0.6495 | 0.5883 | 0.6140 | 0.5995 | 0.5967 | 0.5975 | 0.6566 | 0.6512 | 0.6533 |
| 90-99% | 0.6598 | 0.7100 | 0.6588 | 0.7110 | 0.6764 | 0.6736 | 0.6753 | 0.7320 | 0.7235 | 0.7258 |
| 99-99.9% | 0.7393 | 0.7864 | 0.7385 | 0.8016 | 0.7735 | 0.7346 | 0.7420 | 0.8257 | 0.8063 | 0.8045 |
| Top 0.1% | 0.6809 | 0.7295 | 0.6773 | 0.7904 | 0.7390 | 0.6843 | 0.7020 | 0.7772 | 0.7510 | 0.7291 |

Error summaries for every method (mean over runs):

| Check | transformer | transformer, bridge hidden | transformer, deductions unknown (probe C) | trees split_leaf20 | trees split_leaf50 | trees split_leaf50_uf | trees split_leaf50_wv | trees split_leaf50, bridge hidden | trees split_leaf50_uf, bridge hidden | trees split_leaf50_wv, bridge hidden | Random other household |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Per-record score: energy score on ranks of 20 assets and debts (lower is better) | 0.5420 [0.5415–0.5426] | 0.5957 [0.5950–0.5963] | 0.5425 [0.5409–0.5434] | 0.5653 | 0.5498 | 0.5501 | 0.5497 | 0.6018 | 0.5979 | 0.5992 | 0.7340 |
| Per-record score on the rank of net worth alone (lower is better) | 0.0923 [0.0920–0.0928] | 0.1108 [0.1105–0.1114] | 0.0929 [0.0928–0.0931] | 0.0975 | 0.0934 | 0.0941 | 0.0948 | 0.1081 | 0.1073 | 0.1086 | 0.2010 |
| Mean net worth by income group: average miss (%) | 20.7 [17.9–22.6] | 25.9 [24.3–27.9] | 20.4 [19.5–21.9] | 21.7 | 21.0 | 16.1 | 18.3 | 19.6 | 17.8 | 16.3 | 150.0 |
| Median net worth by income group: average miss (%) | 13.9 [13.4–14.9] | 18.1 [15.2–19.6] | 14.4 [12.6–15.8] | 18.2 | 18.6 | 16.8 | 18.7 | 21.7 | 15.4 | 22.5 | 141.8 |
| Holding rates of 9 items by income group: average gap (points) | 2.57 [2.48–2.71] | 3.20 [2.99–3.31] | 2.63 [2.49–2.93] | 3.51 | 3.06 | 2.20 | 2.57 | 3.63 | 2.96 | 3.85 | 22.89 |
| Mean net worth by age group: average miss (%) | 27.0 [26.1–27.7] | 27.7 [25.2–31.4] | 24.6 [19.5–29.4] | 26.2 | 20.4 | 25.6 | 24.0 | 20.4 | 22.1 | 26.6 | 206.2 |
| Holding rates by age group: average gap (points) | 1.90 [1.82–2.02] | 3.00 [2.67–3.22] | 2.03 [1.97–2.09] | 1.90 | 1.84 | 1.84 | 2.01 | 2.44 | 2.50 | 2.59 | 7.51 |
| Mean net worth by filing status: average miss (%) | 16.0 [13.8–20.0] | 15.8 [13.7–19.0] | 17.1 [14.3–20.5] | 15.5 | 7.6 | 11.4 | 11.8 | 9.3 | 11.6 | 12.4 | 92.0 |
| Holding rates by filing status: average gap (points) | 1.69 [1.62–1.80] | 2.85 [2.66–3.05] | 1.76 [1.56–1.92] | 1.45 | 1.49 | 1.70 | 1.75 | 2.22 | 2.46 | 2.19 | 8.09 |
| Untargeted: holding rates of 20 items by income × age, average gap (points) | 2.39 [2.33–2.43] | 2.92 [2.84–2.97] | 2.43 [2.38–2.51] | 2.50 | 2.36 | 2.36 | 2.44 | 2.68 | 2.75 | 2.89 | 8.35 |
| Untargeted: mean net worth by income × age, average miss (% of group mean) | 35.7 [29.7–39.9] | 38.5 [36.8–40.0] | 33.4 [29.7–35.9] | 24.2 | 22.5 | 22.8 | 26.7 | 21.6 | 30.8 | 27.2 | 227.5 |
| Untargeted: joint holding of 190 pairs of items within income groups, average gap (points) | 2.37 [2.29–2.49] | 2.54 [2.49–2.61] | 2.32 [2.20–2.54] | 3.61 | 3.11 | 2.36 | 2.31 | 3.07 | 2.53 | 2.57 | 8.75 |
| Untargeted: joint holding of 190 pairs of items, all households, average gap (points) | 0.55 [0.52–0.59] | 0.72 [0.66–0.76] | 0.52 [0.48–0.58] | 0.57 | 0.50 | 0.51 | 0.54 | 0.49 | 0.52 | 0.53 | 0.08 |
| Untargeted: rank correlation of net worth with 11 incomes, average gap | 0.022 [0.020–0.024] | 0.021 [0.021–0.021] | 0.023 [0.022–0.024] | 0.020 | 0.022 | 0.024 | 0.027 | 0.012 | 0.021 | 0.019 | 0.237 |
| Untargeted: top 1% share of net worth within income groups, average gap (points) | 5.58 [5.22–5.80] | 5.39 [5.28–5.51] | 5.12 [3.98–6.19] | 4.30 | 3.41 | 4.40 | 4.49 | 3.36 | 4.82 | 4.20 | 22.89 |
| Untargeted: share of homeowners with a mortgage within income groups, average gap (points) | 0.80 [0.59–1.01] | 2.35 [1.41–3.09] | 0.87 [0.74–1.13] | 1.27 | 1.64 | 1.37 | 1.79 | 3.62 | 4.64 | 7.26 | 8.44 |

## PUF records: completed wealth by AGI group against the SCF interpolated to 2015

| | SCF 2015 | SCF sampling error | transformer | transformer, deductions unknown (probe C) | trees split_leaf20 | trees split_leaf50 | trees split_leaf50_uf | trees split_leaf50_wv | trees zero_leaf50 |
|---|---|---|---|---|---|---|---|---|---|
| All returns, mean | $628k | $17k | $508k [$495k–$518k] | $559k [$537k–$596k] | $525k | $498k | $517k | $535k | $439k |
| All returns, median | $91k | $4k | $24k [$22k–$25k] | $56k [$56k–$57k] | $61k | $62k | $62k | $65k | $22k |
| Mean, Bottom 50% | $113k | $5k | $105k [$105k–$105k] | $153k [$144k–$167k] | $121k | $122k | $126k | $131k | $74k |
| Mean, 50-90% | $370k | $13k | $362k [$358k–$370k] | $423k [$407k–$449k] | $410k | $408k | $408k | $417k | $328k |
| Mean, 90-99% | $2.1m | $109k | $2.0m [$1.9m–$2.0m] | $2.0m [$1.9m–$2.1m] | $2.0m | $1.9m | $1.9m | $1.9m | $1.9m |
| Mean, 99-99.9% | $12.3m | $704k | $10.4m [$9.9m–$11.0m] | $10.2m [$9.8m–$10.7m] | $9.8m | $8.6m | $9.0m | $9.9m | $8.5m |
| Mean, Top 0.1% | $54.7m | $3.6m | $38.6m [$37.9m–$39.7m] | $37.8m [$34.0m–$44.5m] | $30.4m | $26.2m | $36.4m | $39.7m | $25.5m |
| Mean, Top 1% | $16.6m | $692k | $13.3m [$12.7m–$13.8m] | $13.0m [$12.3m–$14.1m] | $11.8m | $10.3m | $11.7m | $12.9m | $10.2m |
| Median, Bottom 50% | $17k | $2k | $5k [$5k–$6k] | $10k [$9k–$10k] | $9k | $10k | $10k | $11k | $5k |
| Median, 50-90% | $146k | $9k | $84k [$82k–$86k] | $131k [$125k–$137k] | $136k | $136k | $135k | $137k | $76k |
| Median, 90-99% | $1.1m | $71k | $912k [$903k–$928k] | $924k [$896k–$957k] | $883k | $862k | $900k | $882k | $822k |
| Median, 99-99.9% | $8.0m | $851k | $5.7m [$5.7m–$5.7m] | $5.6m [$5.4m–$5.9m] | $4.5m | $4.1m | $4.7m | $4.4m | $3.9m |
| Median, Top 0.1% | $32.7m | $3.5m | $21.2m [$20.2m–$22.2m] | $20.7m [$19.1m–$22.8m] | $14.4m | $13.3m | $16.0m | $16.8m | $13.0m |
| Median, Top 1% | $9.0m | $720k | $6.3m [$6.3m–$6.4m] | $6.2m [$6.1m–$6.6m] | $4.9m | $4.6m | $5.2m | $4.9m | $4.4m |
| Own a home, all returns (%) | 66.5 | 0.7 | 28.0 [27.7–28.1] | 56.7 [55.7–58.1] | 60.0 | 60.7 | 60.3 | 60.6 | 28.3 |
| Own a home, Bottom 50% (%) | 48.8 | 1.1 | 7.4 [7.2–7.6] | 36.5 [34.9–38.1] | 41.1 | 42.6 | 41.9 | 42.7 | 8.8 |
| Own a home, 50-90% (%) | 77.2 | 0.9 | 39.7 [39.2–40.2] | 73.0 [72.1–74.4] | 75.5 | 75.3 | 75.2 | 75.1 | 38.8 |
| Own a home, 90-99% (%) | 92.0 | 1.0 | 83.5 [83.1–84.1] | 92.4 [92.3–92.5] | 92.4 | 92.2 | 92.3 | 92.0 | 82.4 |
| Own a home, 99-99.9% (%) | 95.3 | 2.1 | 90.1 [89.6–90.4] | 95.1 [94.6–95.3] | 94.8 | 94.7 | 94.7 | 94.4 | 88.8 |
| Own a home, Top 0.1% (%) | 98.6 | 2.1 | 91.5 [91.0–91.8] | 96.6 [95.6–97.3] | 95.5 | 95.3 | 95.4 | 95.0 | 89.6 |
| Hold stocks, all returns (%) | 20.0 | 0.6 | 17.9 [17.4–18.3] | 18.0 [17.6–18.2] | 18.1 | 17.7 | 17.7 | 17.7 | 17.5 |
| Hold stocks, Bottom 50% (%) | 9.1 | 0.6 | 8.1 [7.8–8.4] | 8.3 [8.1–8.5] | 7.9 | 7.7 | 7.4 | 7.3 | 7.3 |
| Hold stocks, 50-90% (%) | 21.3 | 0.9 | 20.9 [20.3–21.6] | 21.0 [20.5–21.4] | 21.8 | 21.0 | 21.2 | 21.2 | 21.0 |
| Hold stocks, 90-99% (%) | 53.1 | 2.0 | 52.3 [50.8–53.2] | 51.4 [50.8–51.7] | 51.8 | 51.6 | 52.6 | 52.6 | 51.8 |
| Hold stocks, 99-99.9% (%) | 83.1 | 2.8 | 81.0 [79.8–82.4] | 79.7 [78.6–80.6] | 77.9 | 77.2 | 79.0 | 78.7 | 77.4 |
| Hold stocks, Top 0.1% (%) | 93.5 | 2.7 | 89.6 [88.4–90.8] | 88.9 [87.8–90.4] | 82.1 | 83.8 | 87.0 | 85.9 | 83.6 |
| Hold a business, all returns (%) | 12.0 | 0.4 | 9.3 [8.9–9.9] | 9.7 [9.1–10.5] | 10.4 | 10.2 | 10.3 | 10.5 | 9.4 |
| Hold a business, Bottom 50% (%) | 6.1 | 0.5 | 4.7 [4.2–5.1] | 5.8 [5.5–6.2] | 6.5 | 6.5 | 6.5 | 6.8 | 5.6 |
| Hold a business, 50-90% (%) | 12.7 | 0.6 | 10.7 [10.2–11.2] | 10.6 [9.7–11.4] | 10.5 | 10.4 | 10.6 | 10.7 | 9.7 |
| Hold a business, 90-99% (%) | 27.5 | 1.7 | 24.3 [22.3–25.9] | 23.0 [21.3–25.1] | 26.6 | 24.4 | 24.7 | 25.4 | 24.0 |
| Hold a business, 99-99.9% (%) | 63.5 | 4.6 | 51.2 [49.8–53.7] | 49.1 [47.1–50.2] | 55.5 | 52.7 | 50.8 | 49.2 | 52.2 |
| Hold a business, Top 0.1% (%) | 85.2 | 6.1 | 73.3 [69.7–76.2] | 70.8 [66.5–74.0] | 72.1 | 69.2 | 68.6 | 64.8 | 68.5 |
| Have a mortgage, all returns (%) | 45.6 | 0.7 | 21.6 [21.5–21.7] | 42.3 [41.0–43.9] | 44.1 | 45.1 | 44.6 | 45.1 | 21.6 |
| Have a mortgage, Bottom 50% (%) | 26.1 | 0.9 | 4.0 [4.0–4.0] | 22.9 [21.2–24.5] | 24.9 | 26.8 | 26.1 | 27.2 | 4.0 |
| Have a mortgage, 50-90% (%) | 58.1 | 1.1 | 31.5 [31.3–31.7] | 58.0 [56.6–59.8] | 59.7 | 59.8 | 59.5 | 59.4 | 31.4 |
| Have a mortgage, 90-99% (%) | 71.3 | 1.5 | 70.1 [69.6–70.7] | 77.7 [77.6–77.8] | 78.1 | 78.4 | 78.0 | 77.8 | 70.5 |
| Have a mortgage, 99-99.9% (%) | 64.1 | 4.0 | 67.9 [67.2–68.8] | 72.0 [71.4–72.6] | 73.5 | 74.6 | 74.0 | 74.1 | 70.0 |
| Have a mortgage, Top 0.1% (%) | 54.6 | 5.3 | 64.7 [64.3–65.4] | 68.6 [67.2–69.4] | 69.2 | 71.5 | 71.7 | 70.9 | 67.2 |
| Top 1% share of net worth (%) | 37.8 | 0.8 | 41.5 [40.4–42.1] | 38.5 [37.5–39.6] | 37.5 | 35.7 | 36.0 | 38.2 | 40.0 |
| Top 0.1% share of net worth (%) | 14.4 | 0.7 | 15.7 [15.0–16.0] | 14.3 [13.3–15.7] | 14.7 | 13.5 | 13.8 | 15.6 | 15.4 |
| All returns with split-off units at zero, mean | $545k | $15k |  |  |  |  |  |  |  |
| All returns with split-off units at zero, median | $57k | $3k |  |  |  |  |  |  |  |
| PUF returns that are not dependent filers, median | | | $29k | $68k | $73k | $74k | $74k | $77k | $27k |
| PUF returns that are not dependent filers, own a home (%) | | | 29.8 | 59.5 | 62.5 | 62.8 | 62.7 | 62.9 | 30.0 |

## Generated records: probe V's full scorecard

### Probe V scorecard tables (written by v_collect.py)

Runs per model: PUF validation: 1 (PUF validation); BL trees, PUF alone (probe BL, minimum leaf 20): 2 (BL trees, PUF alone (probe BL, minimum leaf 20) seed 1, BL trees, PUF alone (probe BL, minimum leaf 20) seed 2); Trees joint_leaf20: 1 (Trees joint_leaf20 seed 1); Trees joint_leaf50: 2 (Trees joint_leaf50 seed 1, Trees joint_leaf50 seed 2); Trees joint_leaf50_uf: 2 (Trees joint_leaf50_uf seed 1, Trees joint_leaf50_uf seed 2); Trees joint_leaf50_wv: 1 (Trees joint_leaf50_wv seed 1); Transformer (probe C, zero coding): 3 (Transformer (probe C, zero coding) seed 1, Transformer (probe C, zero coding) seed 2, Transformer (probe C, zero coding) seed 3)

AGI thresholds from the real PUF training records: {"p50": 36200.0, "p90": 133700.0, "p99": 464600.0, "p999": 2131000.0}

Means over model seeds; range over seeds in brackets. 'Real' is the PUF training records (166,156 records). 'PUF validation' is the 20,768 held-out real records scored as if generated: the size of a miss that sampling alone produces with a file that small.

## Summary

| Check | Real | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| Total AGI ($bn) | 8,094 | 8,029 | 8,238 [8,180–8,297] | 8,193 | 8,184 [8,175–8,193] | 8,184 [8,175–8,193] | 8,193 | 7,819 [7,804–7,845] |
| AGI, top 0.1% group ($bn) | 731 | 716 | 806 [739–873] | 764 | 761 [758–764] | 761 [758–764] | 764 | 665 [616–708] |
| 1. Sum over 30 amounts × 5 groups of |generated − real| ($bn) | 0 | 492 | 390 [316–464] | 383 | 368 [353–383] | 368 [353–383] | 383 | 877 [860–897] |
| 1. Mean gap in share nonzero, 31 amounts × 5 groups (points) | 0.00 | 0.56 | 1.35 [1.33–1.37] | 0.78 | 0.79 [0.78–0.80] | 0.79 [0.78–0.80] | 0.78 | 2.31 [2.13–2.49] |
| 2. Filing status × group, mean gap (points) | 0.00 | 0.35 | 1.21 [1.21–1.21] | 0.28 | 0.25 [0.23–0.28] | 0.25 [0.23–0.28] | 0.28 | 1.97 [1.61–2.46] |
| 2. Filing status × group, largest gap (points) | 0.0 | 0.9 | 5.6 [5.5–5.6] | 1.0 | 0.9 [0.7–1.0] | 0.9 [0.7–1.0] | 1.0 | 8.4 [6.1–12.4] |
| 2. Itemizer share, top 0.1% (%) | 92.8 | 91.4 | 93.9 [93.7–94.1] | 94.0 | 93.9 [93.9–94.0] | 93.9 [93.9–94.0] | 94.0 | 86.2 [85.1–87.2] |
| 2. Itemizer share, 50–90 (%) | 42.8 | 42.9 | 42.8 [42.7–42.9] | 42.9 | 42.9 [42.9–42.9] | 42.9 [42.9–42.9] | 42.9 | 41.0 [40.6–41.5] |
| 2. Co-holding of 28 pairs × 5 groups, mean gap (points) | 0.00 | 0.56 | 1.17 [1.11–1.22] | 0.89 | 0.90 [0.89–0.92] | 0.90 [0.89–0.92] | 0.89 | 2.49 [2.19–2.77] |
| 2. Co-holding, largest gap (points) | 0.0 | 2.8 | 7.1 [6.8–7.4] | 3.0 | 3.1 [3.0–3.1] | 3.1 [3.0–3.1] | 3.0 | 14.3 [11.4–19.4] |
| 2. Any EITC children, bottom 50 (%) | 23.8 | 24.8 | 23.2 [23.2–23.2] | 23.2 | 23.1 [23.1–23.2] | 23.1 [23.1–23.2] | 23.2 | 22.2 [21.8–22.7] |
| 2. Child-credit children per return, 50–90 | 0.483 | 0.480 | 0.482 [0.482–0.482] | 0.482 | 0.480 [0.478–0.482] | 0.480 [0.478–0.482] | 0.482 | 0.468 [0.463–0.473] |
| 2. Age range × group, bottom three groups, mean gap (points) | 0.00 | 0.57 | 0.24 [0.23–0.26] | 0.33 | 0.34 [0.33–0.34] | 0.34 [0.33–0.34] | 0.33 | 0.61 [0.57–0.65] |
| 3. Returns breaking any rule real records never break (%) | 0.00 | 0.02 | 7.86 [7.83–7.89] | 0.30 | 0.30 [0.30–0.30] | 0.30 [0.30–0.30] | 0.30 | 1.00 [0.93–1.07] |
| 3. Taxable pensions above total pensions (%) | 0.00 | 0.00 | 5.78 [5.76–5.81] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] |
| 3. Same, by more than 5% (%) | 0.00 | 0.00 | 1.23 [1.20–1.25] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] |
| 3. Qualified dividends above ordinary dividends (%) | 0.00 | 0.00 | 2.02 [2.01–2.03] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] |
| 3. Same, by more than 5% (%) | 0.00 | 0.00 | 0.75 [0.75–0.76] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] |
| 3. Any rule broken, top 0.1% group (%) | 0.00 | 0.00 | 16.18 [16.16–16.19] | 0.11 | 0.10 [0.09–0.11] | 0.10 [0.09–0.11] | 0.11 | 1.83 [1.74–1.97] |
| 4. Net income tax, all returns ($bn) | 1,095 | 1,084 | 1,129 [1,112–1,146] | 1,134 | 1,132 [1,130–1,134] | 1,132 [1,130–1,134] | 1,134 | 1,093 [1,077–1,109] |
| 4. Net income tax, top 0.1% group ($bn) | 204 | 200 | 219 [204–234] | 215 | 214 [213–215] | 214 [213–215] | 215 | 204 [189–216] |
| 4. Top 1% share of net income tax (%) | 39.7 | 39.9 | 40.9 [39.9–41.8] | 40.8 | 40.9 [40.8–40.9] | 40.9 [40.8–40.9] | 40.8 | 39.8 [38.4–40.8] |
| 4. Largest gap in average tax rate by group (points) | 0.00 | 0.15 | 0.76 [0.34–1.18] | 0.20 | 0.19 [0.19–0.20] | 0.19 [0.19–0.20] | 0.20 | 2.69 [2.45–2.91] |
| 4. Tax-Calculator: net income tax, all returns ($bn) | — | — | — | — | — | — | — | — |
| 4. Tax-Calculator: net income tax, top 0.1% group ($bn) | — | — | — | — | — | — | — | — |
| 4. Tax-Calculator: largest gap in average tax rate by group (points) | 0.00 | — | — | — | — | — | — | — |
| 5. Classifier area under ROC curve, all returns (0.5 = indistinguishable) | 0.500 | 0.501 | 0.540 [0.540–0.540] | 0.562 | 0.562 [0.561–0.562] | 0.562 [0.561–0.562] | 0.562 | 0.814 [0.812–0.816] |
| 5. Same, within the top 0.1% group | 0.500 | 0.507 | 0.583 [0.581–0.584] | 0.646 | 0.648 [0.646–0.651] | 0.648 [0.646–0.651] | 0.646 | 0.802 [0.797–0.808] |
| 5. Same classifier on rank-band tokens instead of dollars, all returns | 0.500 | 0.502 | 0.530 [0.526–0.534] | — | — | — | — | — |
| 5. Tokens, within the top 0.1% group | 0.500 | 0.502 | 0.598 [0.582–0.614] | — | — | — | — | — |
| 6. Share at distance zero from a training record, all 37 variables (%) | — | 26.65 | 28.91 [28.91–28.91] | 28.26 | 28.52 [28.26–28.78] | 28.52 [28.26–28.78] | 28.26 | 25.65 [25.31–26.13] |
| 6. Same, top 0.1% group (%) | — | 0.08 | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.10 [0.00–0.16] |
| 6. Median distance to the nearest training record, top 0.1% group | — | 0.444 | 0.562 [0.558–0.566] | 0.510 | 0.510 [0.510–0.510] | 0.510 [0.510–0.510] | 0.510 | 0.566 [0.557–0.582] |
| 6. Share with a token pattern identical to a training record (%) | — | 34.2 | 35.3 [35.3–35.3] | — | — | — | — | — |
| 6. Same, top 0.1% group (%) | — | 0.1 | 0.0 [0.0–0.0] | — | — | — | — | — |

## 1. Every variable: total dollars by AGI group ($bn), real / PUF validation / BL trees, PUF alone (probe BL, minimum leaf 20) / Trees joint_leaf20 / Trees joint_leaf50 / Trees joint_leaf50_uf / Trees joint_leaf50_wv / Transformer (probe C, zero coding)

| Variable | bottom 50 | 50-90 | 90-99 | 99-99.9 | top 0.1 | all |
|---|---|---|---|---|---|---|
| wages (E00200) | 810 / 827 / 808 / 814 / 813 / 813 / 814 / 820 | 2,654 / 2,601 / 2,653 / 2,646 / 2,647 / 2,647 / 2,646 / 2,644 | 1,590 / 1,581 / 1,592 / 1,596 / 1,590 / 1,590 / 1,596 / 1,562 | 455 / 456 / 460 / 463 / 462 / 462 / 463 / 415 | 211 / 220 / 213 / 212 / 212 / 212 / 212 / 184 | 5,721 / 5,685 / 5,726 / 5,730 / 5,724 / 5,724 / 5,730 / 5,625 |
| Schedule C (E00900) | 59 / 59 / 57 / 55 / 56 / 56 / 55 / 50 | 83 / 75 / 87 / 80 / 80 / 80 / 80 / 68 | 82 / 79 / 92 / 85 / 85 / 85 / 85 / 74 | 33 / 31 / 45 / 36 / 36 / 36 / 36 / 36 | 10 / 6 / 13 / 11 / 11 / 11 / 11 / 12 | 266 / 250 / 295 / 267 / 268 / 268 / 267 / 240 |
| Schedule E total (E02000) | -33 / -26 / -31 / -37 / -38 / -38 / -37 / -68 | 47 / 58 / 45 / 46 / 45 / 45 / 46 / 42 | 153 / 154 / 151 / 162 / 159 / 159 / 162 / 142 | 205 / 210 / 205 / 221 / 220 / 220 / 221 / 210 | 190 / 181 / 181 / 195 / 194 / 194 / 195 / 207 | 563 / 578 / 551 / 587 / 580 / 580 / 587 / 532 |
| partnership and S corp (E26270) | -31 / -27 / -32 / -34 / -34 / -34 / -34 / -59 | 38 / 46 / 36 / 36 / 35 / 35 / 36 / 32 | 129 / 130 / 127 / 129 / 128 / 128 / 129 / 117 | 186 / 195 / 188 / 193 / 191 / 191 / 193 / 188 | 175 / 167 / 166 / 174 / 173 / 173 / 174 / 188 | 496 / 509 / 485 / 498 / 493 / 493 / 498 / 467 |
| long-term gains (P23250) | 20 / 19 / 17 / 18 / 17 / 17 / 18 / 7 | 38 / 42 / 38 / 35 / 35 / 35 / 35 / 35 | 94 / 89 / 97 / 91 / 92 / 92 / 91 / 82 | 111 / 108 / 126 / 118 / 117 / 117 / 118 / 106 | 248 / 242 / 295 / 260 / 258 / 258 / 260 / 197 | 510 / 500 / 573 / 522 / 519 / 519 / 522 / 427 |
| short-term gains (P22250) | -7 / -6 / -16 / -6 / -6 / -6 / -6 / -6 | -7 / -5 / -7 / -8 / -8 / -8 / -8 / -9 | -10 / -8 / -12 / -11 / -11 / -11 / -11 / -10 | -7 / -8 / -8 / -9 / -8 / -8 / -9 / -7 | -3 / -4 / -2 / 1 / 1 / 1 / 1 / 0 | -35 / -30 / -46 / -32 / -32 / -32 / -32 / -31 |
| ordinary dividends (E00600) | 14 / 14 / 15 / 15 / 15 / 15 / 15 / 15 | 41 / 40 / 39 / 37 / 38 / 38 / 37 / 41 | 55 / 48 / 61 / 63 / 63 / 63 / 63 / 48 | 39 / 41 / 46 / 48 / 48 / 48 / 48 / 36 | 45 / 47 / 57 / 49 / 49 / 49 / 49 / 40 | 195 / 190 / 217 / 212 / 212 / 212 / 212 / 179 |
| qualified dividends (E00650) | 9 / 9 / 10 / 10 / 10 / 10 / 10 / 10 | 31 / 30 / 29 / 27 / 28 / 28 / 27 / 31 | 43 / 38 / 47 / 51 / 50 / 50 / 51 / 37 | 31 / 34 / 37 / 40 / 40 / 40 / 40 / 28 | 36 / 39 / 44 / 40 / 40 / 40 / 40 / 35 | 151 / 150 / 167 / 167 / 167 / 167 / 167 / 141 |
| taxable interest (E00300) | 9 / 10 / 9 / 9 / 9 / 9 / 9 / 8 | 16 / 16 / 15 / 15 / 16 / 16 / 15 / 17 | 16 / 17 / 19 / 18 / 19 / 19 / 18 / 17 | 12 / 13 / 16 / 14 / 14 / 14 / 14 / 15 | 18 / 16 / 23 / 19 / 19 / 19 / 19 / 9 | 71 / 71 / 82 / 76 / 76 / 76 / 76 / 65 |
| total pensions (E01500) | 122 / 131 / 121 / 124 / 123 / 123 / 124 / 127 | 462 / 504 / 458 / 442 / 449 / 449 / 442 / 446 | 310 / 255 / 307 / 319 / 315 / 315 / 319 / 255 | 42 / 48 / 46 / 54 / 55 / 55 / 54 / 35 | 7 / 7 / 7 / 10 / 10 / 10 / 10 / 4 | 944 / 946 / 939 / 949 / 953 / 953 / 949 / 868 |
| taxable pensions (E01700) | 86 / 90 / 86 / 85 / 85 / 85 / 85 / 78 | 307 / 332 / 305 / 302 / 301 / 301 / 302 / 299 | 148 / 130 / 153 / 159 / 159 / 159 / 159 / 144 | 11 / 10 / 13 / 18 / 18 / 18 / 18 / 13 | 1 / 1 / 1 / 2 / 2 / 2 / 2 / 1 | 553 / 563 / 558 / 566 / 565 / 565 / 566 / 535 |
| taxable IRA (E01400) | 26 / 24 / 26 / 25 / 25 / 25 / 25 / 22 | 93 / 96 / 92 / 91 / 92 / 92 / 91 / 89 | 68 / 69 / 76 / 74 / 75 / 75 / 74 / 64 | 11 / 11 / 16 / 20 / 21 / 21 / 20 / 12 | 1 / 1 / 2 / 2 / 2 / 2 / 2 / 2 | 201 / 202 / 213 / 213 / 216 / 216 / 213 / 189 |
| gross Social Security (E02400) | 210 / 196 / 206 / 206 / 205 / 205 / 206 / 189 | 209 / 216 / 209 / 210 / 211 / 211 / 210 / 227 | 61 / 56 / 64 / 66 / 66 / 66 / 66 / 59 | 6 / 6 / 7 / 8 / 8 / 8 / 8 / 7 | 1 / 1 / 1 / 1 / 1 / 1 / 1 / 1 | 487 / 474 / 487 / 490 / 490 / 490 / 490 / 482 |
| rent and royalty income (E25850) | 12 / 14 / 12 / 14 / 13 / 13 / 14 / 8 | 24 / 24 / 24 / 27 / 27 / 27 / 27 / 23 | 28 / 26 / 30 / 36 / 36 / 36 / 36 / 28 | 17 / 15 / 18 / 24 / 24 / 24 / 24 / 18 | 8 / 8 / 9 / 10 / 10 / 10 / 10 / 6 | 89 / 86 / 93 / 111 / 110 / 110 / 111 / 83 |
| rent and royalty loss (E25860) | 13 / 12 / 13 / 16 / 17 / 17 / 16 / 16 | 19 / 18 / 19 / 20 / 20 / 20 / 20 / 16 | 10 / 11 / 12 / 12 / 12 / 12 / 12 / 9 | 4 / 4 / 5 / 6 / 6 / 6 / 6 / 3 | 2 / 1 / 3 / 2 / 2 / 2 / 2 / 1 | 48 / 47 / 52 / 56 / 57 / 57 / 56 / 45 |
| Schedule F (E02100) | -9 / -7 / -9 / -12 / -12 / -12 / -12 / -12 | -3 / -5 / -2 / -4 / -4 / -4 / -4 / -2 | 0 / -2 / -0 / -0 / 0 / 0 / -0 / 1 | -0 / 0 / -1 / 0 / 0 / 0 / 0 / 0 | -0 / -1 / -1 / 0 / -0 / -0 / 0 / 0 | -12 / -15 / -13 / -16 / -16 / -16 / -16 / -12 |
| tax-exempt interest (E00400) | 4 / 4 / 4 / 5 / 5 / 5 / 5 / 5 | 11 / 8 / 11 / 12 / 12 / 12 / 12 / 11 | 15 / 14 / 20 / 22 / 22 / 22 / 22 / 12 | 11 / 11 / 16 / 16 / 16 / 16 / 16 / 9 | 8 / 8 / 12 / 10 / 10 / 10 / 10 / 6 | 49 / 46 / 62 / 65 / 66 / 66 / 65 / 43 |
| estate and trust income (E26390) | 1 / 1 / 1 / 1 / 1 / 1 / 1 / 1 | 3 / 5 / 3 / 2 / 2 / 2 / 2 / 2 | 6 / 10 / 5 / 8 / 7 / 7 / 8 / 5 | 6 / 5 / 6 / 10 / 11 / 11 / 10 / 7 | 10 / 9 / 7 / 14 / 14 / 14 / 14 / 13 | 25 / 29 / 23 / 36 / 36 / 36 / 36 / 28 |
| estate and trust loss (E26400) | 1 / 1 / 0 / 2 / 2 / 2 / 2 / 2 | 0 / 0 / 0 / 0 / 0 / 0 / 0 / 0 | 1 / 1 / 0 / 1 / 1 / 1 / 1 / 0 | 0 / 1 / 1 / 1 / 1 / 1 / 1 / 0 | 1 / 1 / 1 / 1 / 1 / 1 / 1 / 0 | 3 / 3 / 3 / 5 / 5 / 5 / 5 / 3 |
| capital gain distributions (E01100) | 1 / 1 / 2 / 2 / 2 / 2 / 2 / 1 | 4 / 5 / 4 / 4 / 4 / 4 / 4 / 3 | 4 / 3 / 4 / 3 / 3 / 3 / 3 / 2 | 1 / 0 / 2 / 1 / 1 / 1 / 1 / 0 | 0 / 0 / 0 / 0 / 0 / 0 / 0 / 0 | 9 / 9 / 12 / 10 / 10 / 10 / 10 / 7 |
| unemployment (E02300) | 9 / 10 / 8 / 8 / 8 / 8 / 8 / 7 | 11 / 10 / 11 / 11 / 11 / 11 / 11 / 10 | 2 / 1 / 3 / 2 / 2 / 2 / 2 / 2 | 0 / 0 / 0 / 0 / 0 / 0 / 0 / 0 | 0 / 0 / 0 / 0 / 0 / 0 / 0 / 0 | 21 / 21 / 23 / 22 / 22 / 22 / 22 / 19 |
| IRA deduction (E03150) | 1 / 1 / 2 / 2 / 2 / 2 / 2 / 2 | 6 / 6 / 5 / 6 / 6 / 6 / 6 / 5 | 2 / 3 / 3 / 3 / 3 / 3 / 3 / 2 | 0 / 0 / 1 / 0 / 0 / 0 / 0 / 0 | 0 / 0 / 0 / 0 / 0 / 0 / 0 / 0 | 10 / 10 / 11 / 11 / 11 / 11 / 11 / 9 |
| student loan interest (E03210) | 3 / 3 / 3 / 3 / 3 / 3 / 3 / 3 | 8 / 8 / 8 / 7 / 8 / 8 / 7 / 6 | 0 / 0 / 1 / 1 / 1 / 1 / 1 / 1 | 0 / 0 / 0 / 0 / 0 / 0 / 0 / 0 | 0 / 0 / 0 / 0 / 0 / 0 / 0 / 0 | 11 / 11 / 11 / 11 / 11 / 11 / 11 / 10 |
| self-employed health insurance (E03270) | 3 / 3 / 3 / 4 / 4 / 4 / 4 / 5 | 8 / 7 / 8 / 8 / 8 / 8 / 8 / 6 | 8 / 7 / 9 / 8 / 8 / 8 / 8 / 6 | 3 / 3 / 4 / 3 / 3 / 3 / 3 / 3 | 1 / 1 / 1 / 1 / 1 / 1 / 1 / 0 | 23 / 22 / 25 / 24 / 24 / 24 / 24 / 21 |
| medical (E17500) | 27 / 21 / 29 / 31 / 31 / 31 / 31 / 25 | 63 / 63 / 60 / 61 / 62 / 62 / 61 / 52 | 16 / 12 / 19 / 18 / 17 / 17 / 18 / 16 | 1 / 1 / 3 / 2 / 2 / 2 / 2 / 2 | 0 / 0 / 1 / 0 / 0 / 0 / 0 / 0 | 107 / 98 / 112 / 112 / 112 / 112 / 112 / 96 |
| state and local income/sales tax (E18400) | 5 / 4 / 7 / 7 / 7 / 7 / 7 / 16 | 71 / 70 / 72 / 73 / 73 / 73 / 73 / 77 | 98 / 97 / 98 / 97 / 96 / 96 / 97 / 93 | 50 / 50 / 53 / 51 / 50 / 50 / 51 / 45 | 50 / 50 / 55 / 49 / 49 / 49 / 49 / 23 | 274 / 271 / 284 / 276 / 275 / 275 / 276 / 254 |
| real estate tax (E18500) | 10 / 10 / 12 / 12 / 12 / 12 / 12 / 15 | 65 / 64 / 66 / 65 / 65 / 65 / 65 / 66 | 58 / 56 / 59 / 57 / 57 / 57 / 57 / 51 | 14 / 14 / 15 / 15 / 15 / 15 / 15 / 11 | 4 / 4 / 4 / 4 / 4 / 4 / 4 / 2 | 152 / 148 / 156 / 154 / 153 / 153 / 154 / 146 |
| interest paid (E19200) | 16 / 17 / 18 / 18 / 18 / 18 / 18 / 23 | 118 / 119 / 118 / 117 / 118 / 118 / 117 / 118 | 87 / 84 / 87 / 86 / 86 / 86 / 86 / 79 | 16 / 17 / 17 / 17 / 17 / 17 / 17 / 12 | 6 / 5 / 7 / 6 / 6 / 6 / 6 / 2 | 244 / 242 / 247 / 245 / 245 / 245 / 245 / 234 |
| cash charity (E19800) | 7 / 4 / 9 / 9 / 9 / 9 / 9 / 14 | 45 / 44 / 46 / 47 / 47 / 47 / 47 / 46 | 39 / 37 / 40 / 38 / 38 / 38 / 38 / 34 | 17 / 16 / 19 / 17 / 17 / 17 / 17 / 15 | 17 / 17 / 24 / 17 / 17 / 17 / 17 / 6 | 126 / 118 / 137 / 128 / 129 / 129 / 128 / 116 |
| noncash charity (E20100) | 1 / 1 / 2 / 2 / 2 / 2 / 2 / 7 | 11 / 11 / 10 / 11 / 10 / 10 / 11 / 12 | 9 / 9 / 10 / 10 / 10 / 10 / 10 / 11 | 7 / 12 / 7 / 7 / 7 / 7 / 7 / 11 | 17 / 14 / 22 / 16 / 16 / 16 / 16 / 3 | 45 / 46 / 52 / 46 / 46 / 46 / 46 / 43 |
| AGI remainder (AGI_REMAINDER) | -165 / -181 / -162 / -166 / -165 / -165 / -166 / -146 | 138 / 143 / 131 / 130 / 130 / 130 / 130 / 118 | 40 / 32 / 40 / 44 / 45 / 45 / 44 / 42 | -2 / -0 / -3 / -0 / -1 / -1 / -0 / 6 | 7 / 4 / 21 / 8 / 7 / 7 / 8 / 11 | 17 / -3 / 26 / 16 / 17 / 17 / 16 / 31 |
| AGI (E00100) | 830 / 840 / 825 / 816 / 816 / 816 / 816 / 775 | 3,415 / 3,398 / 3,404 / 3,380 / 3,382 / 3,382 / 3,380 / 3,355 | 2,245 / 2,198 / 2,278 / 2,295 / 2,286 / 2,286 / 2,295 / 2,175 | 873 / 877 / 925 / 939 / 939 / 939 / 939 / 849 | 731 / 716 / 806 / 764 / 761 / 761 / 764 / 665 | 8,094 / 8,029 / 8,238 / 8,193 / 8,184 / 8,184 / 8,193 / 7,819 |

## 1. Every variable: share nonzero by AGI group (%), real / PUF validation / BL trees, PUF alone (probe BL, minimum leaf 20) / Trees joint_leaf20 / Trees joint_leaf50 / Trees joint_leaf50_uf / Trees joint_leaf50_wv / Transformer (probe C, zero coding)

| Variable | bottom 50 | 50-90 | 90-99 | 99-99.9 | top 0.1 | all |
|---|---|---|---|---|---|---|
| wages (E00200) | 78.3 / 78.2 / 78.2 / 78.3 / 78.2 / 78.2 / 78.3 / 78.6 | 87.1 / 86.5 / 87.3 / 87.4 / 87.3 / 87.3 / 87.4 / 87.0 | 88.5 / 89.5 / 88.6 / 88.7 / 88.6 / 88.6 / 88.7 / 89.9 | 85.9 / 86.4 / 85.0 / 84.6 / 84.6 / 84.6 / 84.6 / 85.9 | 82.7 / 82.6 / 82.9 / 82.4 / 82.5 / 82.5 / 82.4 / 80.4 | 82.8 / 82.6 / 82.8 / 82.9 / 82.9 / 82.9 / 82.9 / 83.0 |
| Schedule C (E00900) | 16.2 / 16.5 / 16.0 / 16.0 / 16.0 / 16.0 / 16.0 / 14.6 | 15.3 / 14.0 / 15.6 / 15.4 / 15.4 / 15.4 / 15.4 / 14.4 | 21.7 / 22.2 / 23.2 / 21.9 / 22.0 / 22.0 / 21.9 / 22.0 | 25.7 / 25.7 / 29.4 / 28.1 / 27.9 / 27.9 / 28.1 / 33.2 | 25.6 / 26.0 / 31.8 / 27.8 / 27.8 / 27.8 / 27.8 / 38.4 | 16.4 / 16.1 / 16.6 / 16.4 / 16.4 / 16.4 / 16.4 / 15.3 |
| Schedule E total (E02000) | 5.7 / 5.6 / 5.8 / 5.8 / 5.9 / 5.9 / 5.8 / 5.3 | 13.8 / 13.6 / 13.4 / 14.6 / 14.6 / 14.6 / 14.6 / 12.7 | 30.4 / 29.9 / 30.8 / 34.4 / 34.2 / 34.2 / 34.4 / 31.7 | 65.2 / 64.9 / 66.0 / 71.2 / 71.1 / 71.1 / 71.2 / 67.3 | 85.2 / 85.0 / 86.6 / 89.5 / 89.4 / 89.4 / 89.5 / 83.4 | 11.8 / 11.5 / 11.8 / 12.6 / 12.6 / 12.6 / 12.6 / 11.2 |
| partnership and S corp (E26270) | 2.2 / 2.2 / 2.2 / 2.2 / 2.2 / 2.2 / 2.2 / 2.2 | 5.7 / 6.5 / 5.6 / 5.8 / 5.8 / 5.8 / 5.8 / 5.4 | 20.7 / 20.5 / 20.5 / 20.4 / 20.3 / 20.3 / 20.4 / 18.8 | 59.1 / 59.8 / 58.2 / 58.4 / 58.1 / 58.1 / 58.4 / 54.0 | 81.9 / 81.5 / 81.2 / 81.8 / 81.6 / 81.6 / 81.8 / 74.3 | 5.9 / 6.1 / 5.8 / 5.9 / 5.9 / 5.9 / 5.9 / 5.4 |
| long-term gains (P23250) | 4.7 / 4.4 / 4.8 / 4.8 / 4.8 / 4.8 / 4.8 / 4.4 | 11.8 / 12.2 / 11.6 / 11.5 / 11.5 / 11.5 / 11.5 / 11.7 | 34.4 / 34.9 / 34.5 / 34.5 / 34.6 / 34.6 / 34.5 / 32.2 | 71.8 / 73.5 / 71.0 / 71.1 / 71.3 / 71.3 / 71.1 / 68.2 | 90.5 / 89.6 / 89.3 / 90.3 / 90.4 / 90.4 / 90.3 / 84.3 | 10.9 / 10.9 / 11.0 / 10.9 / 10.9 / 10.9 / 10.9 / 10.4 |
| short-term gains (P22250) | 2.8 / 2.6 / 2.9 / 2.9 / 3.0 / 3.0 / 2.9 / 2.7 | 7.4 / 7.6 / 7.5 / 7.7 / 7.7 / 7.7 / 7.7 / 7.3 | 25.2 / 26.4 / 25.8 / 26.0 / 26.0 / 26.0 / 26.0 / 22.0 | 58.4 / 59.2 / 57.6 / 57.6 / 57.8 / 57.8 / 57.6 / 50.8 | 79.6 / 80.3 / 78.0 / 80.1 / 80.1 / 80.1 / 80.1 / 69.2 | 7.2 / 7.3 / 7.4 / 7.5 / 7.6 / 7.6 / 7.5 / 6.7 |
| ordinary dividends (E00600) | 8.8 / 8.6 / 9.2 / 9.6 / 9.5 / 9.5 / 9.6 / 9.0 | 21.5 / 21.8 / 21.5 / 22.0 / 22.0 / 22.0 / 22.0 / 21.4 | 51.1 / 52.2 / 51.3 / 52.8 / 52.9 / 52.9 / 52.8 / 47.1 | 81.2 / 79.9 / 80.1 / 82.5 / 82.7 / 82.7 / 82.5 / 77.3 | 91.4 / 91.3 / 90.5 / 93.7 / 93.8 / 93.8 / 93.7 / 87.6 | 18.5 / 18.4 / 18.7 / 19.2 / 19.2 / 19.2 / 19.2 / 17.9 |
| qualified dividends (E00650) | 8.1 / 7.8 / 8.5 / 8.6 / 8.6 / 8.6 / 8.6 / 8.3 | 20.0 / 19.9 / 20.0 / 20.0 / 20.0 / 20.0 / 20.0 / 19.9 | 48.8 / 49.9 / 48.8 / 49.3 / 49.4 / 49.4 / 49.3 / 44.5 | 79.1 / 77.7 / 77.6 / 78.9 / 79.0 / 79.0 / 78.9 / 74.9 | 89.6 / 90.1 / 88.9 / 90.8 / 90.9 / 90.9 / 90.8 / 85.6 | 17.2 / 17.0 / 17.5 / 17.6 / 17.6 / 17.6 / 17.6 / 16.7 |
| taxable interest (E00300) | 14.7 / 14.7 / 15.3 / 15.4 / 15.4 / 15.4 / 15.4 / 15.2 | 35.3 / 34.0 / 35.2 / 35.1 / 35.2 / 35.2 / 35.1 / 35.3 | 67.1 / 68.0 / 67.2 / 67.4 / 67.3 / 67.3 / 67.4 / 64.3 | 91.1 / 92.4 / 90.2 / 90.1 / 90.3 / 90.3 / 90.1 / 83.7 | 98.3 / 98.1 / 98.4 / 98.0 / 98.0 / 98.0 / 98.0 / 89.0 | 28.4 / 27.9 / 28.8 / 28.8 / 28.8 / 28.8 / 28.8 / 28.1 |
| total pensions (E01500) | 13.8 / 14.2 / 13.9 / 14.0 / 13.9 / 13.9 / 14.0 / 13.5 | 25.9 / 26.7 / 25.8 / 26.0 / 26.0 / 26.0 / 26.0 / 26.3 | 32.8 / 30.5 / 33.2 / 33.5 / 33.7 / 33.7 / 33.5 / 32.5 | 25.5 / 24.5 / 27.2 / 28.4 / 28.5 / 28.5 / 28.4 / 28.9 | 24.7 / 23.8 / 25.6 / 26.3 / 26.3 / 26.3 / 26.3 / 25.7 | 20.5 / 20.7 / 20.5 / 20.7 / 20.7 / 20.7 / 20.7 / 20.4 |
| taxable pensions (E01700) | 13.1 / 13.5 / 13.2 / 13.2 / 13.1 / 13.1 / 13.2 / 12.3 | 23.9 / 25.0 / 23.8 / 23.7 / 23.7 / 23.7 / 23.7 / 23.8 | 27.5 / 25.6 / 28.2 / 28.7 / 28.9 / 28.9 / 28.7 / 27.8 | 18.4 / 16.4 / 19.9 / 21.7 / 21.8 / 21.8 / 21.7 / 22.1 | 17.3 / 17.4 / 17.9 / 19.3 / 19.3 / 19.3 / 19.3 / 18.9 | 18.8 / 19.1 / 18.8 / 18.9 / 18.9 / 18.9 / 18.9 / 18.3 |
| taxable IRA (E01400) | 5.9 / 5.6 / 6.0 / 5.8 / 5.8 / 5.8 / 5.8 / 5.3 | 12.1 / 11.9 / 11.8 / 12.0 / 12.1 / 12.1 / 12.0 / 12.2 | 17.1 / 16.9 / 18.9 / 17.9 / 18.0 / 18.0 / 17.9 / 17.3 | 15.6 / 16.2 / 21.5 / 18.7 / 19.1 / 19.1 / 18.7 / 18.3 | 14.0 / 12.9 / 21.1 / 15.8 / 15.9 / 15.9 / 15.8 / 18.7 | 9.5 / 9.2 / 9.6 / 9.5 / 9.6 / 9.6 / 9.5 / 9.3 |
| gross Social Security (E02400) | 18.1 / 17.7 / 18.0 / 17.8 / 17.7 / 17.7 / 17.8 / 16.5 | 19.3 / 20.6 / 19.1 / 19.0 / 19.1 / 19.1 / 19.0 / 20.4 | 19.4 / 17.6 / 20.3 / 20.3 / 20.4 / 20.4 / 20.3 / 20.0 | 17.4 / 17.1 / 20.2 / 20.2 / 20.4 / 20.4 / 20.2 / 21.5 | 18.2 / 15.4 / 24.2 / 20.5 / 20.3 / 20.3 / 20.5 / 23.8 | 18.7 / 18.8 / 18.7 / 18.5 / 18.5 / 18.5 / 18.5 / 18.4 |
| rent and royalty income (E25850) | 2.5 / 2.5 / 2.6 / 2.6 / 2.7 / 2.7 / 2.6 / 2.3 | 5.7 / 5.1 / 5.6 / 6.1 / 6.1 / 6.1 / 6.1 / 5.6 | 13.7 / 13.7 / 14.6 / 15.0 / 14.9 / 14.9 / 15.0 / 13.5 | 27.5 / 26.9 / 31.2 / 29.8 / 30.0 / 30.0 / 29.8 / 27.2 | 45.4 / 41.9 / 45.0 / 46.3 / 46.6 / 46.6 / 46.3 / 32.7 | 5.1 / 4.8 / 5.2 / 5.5 / 5.5 / 5.5 / 5.5 / 4.8 |
| rent and royalty loss (E25860) | 1.9 / 1.8 / 2.0 / 1.9 / 1.9 / 1.9 / 1.9 / 1.9 | 4.9 / 4.8 / 4.9 / 5.1 / 5.1 / 5.1 / 5.1 / 4.2 | 8.2 / 8.2 / 9.9 / 9.4 / 9.4 / 9.4 / 9.4 / 9.3 | 15.2 / 15.6 / 21.1 / 18.2 / 18.0 / 18.0 / 18.2 / 16.5 | 23.1 / 22.6 / 30.6 / 26.9 / 27.0 / 27.0 / 26.9 / 21.1 | 3.8 / 3.7 / 4.1 / 4.0 / 4.0 / 4.0 / 4.0 / 3.6 |
| Schedule F (E02100) | 0.8 / 0.8 / 0.8 / 0.7 / 0.6 / 0.6 / 0.7 / 0.8 | 1.5 / 1.4 / 1.5 / 1.5 / 1.5 / 1.5 / 1.5 / 1.2 | 1.9 / 2.2 / 2.6 / 2.7 / 2.7 / 2.7 / 2.7 / 2.1 | 2.7 / 2.9 / 4.9 / 4.2 / 4.3 / 4.3 / 4.2 / 4.4 | 3.7 / 3.4 / 7.9 / 5.0 / 5.0 / 5.0 / 5.0 / 5.7 | 1.2 / 1.2 / 1.3 / 1.2 / 1.2 / 1.2 / 1.2 / 1.1 |
| tax-exempt interest (E00400) | 1.3 / 1.1 / 1.3 / 1.4 / 1.4 / 1.4 / 1.4 / 1.4 | 4.1 / 3.7 / 4.0 / 4.0 / 4.0 / 4.0 / 4.0 / 3.9 | 13.8 / 13.6 / 15.8 / 14.4 / 14.4 / 14.4 / 14.4 / 11.5 | 38.5 / 35.8 / 42.9 / 38.7 / 38.6 / 38.6 / 38.7 / 34.9 | 63.6 / 62.5 / 68.0 / 63.6 / 64.0 / 64.0 / 63.6 / 53.1 | 3.9 / 3.6 / 4.1 / 4.0 / 4.1 / 4.1 / 4.0 / 3.6 |
| estate and trust income (E26390) | 0.1 / 0.2 / 0.1 / 0.1 / 0.1 / 0.1 / 0.1 / 0.1 | 0.4 / 0.4 / 0.4 / 0.3 / 0.3 / 0.3 / 0.3 / 0.4 | 1.4 / 1.6 / 1.6 / 1.5 / 1.5 / 1.5 / 1.5 / 1.1 | 3.6 / 3.5 / 5.7 / 4.9 / 4.9 / 4.9 / 4.9 / 3.2 | 9.1 / 8.7 / 12.7 / 11.1 / 11.1 / 11.1 / 11.1 / 5.9 | 0.4 / 0.4 / 0.5 / 0.4 / 0.4 / 0.4 / 0.4 / 0.3 |
| estate and trust loss (E26400) | 0.0 / 0.1 / 0.0 / 0.0 / 0.0 / 0.0 / 0.0 / 0.0 | 0.0 / 0.1 / 0.0 / 0.0 / 0.0 / 0.0 / 0.0 / 0.0 | 0.3 / 0.1 / 0.3 / 0.2 / 0.3 / 0.3 / 0.2 / 0.1 | 1.0 / 1.0 / 2.0 / 1.2 / 1.2 / 1.2 / 1.2 / 0.3 | 4.2 / 4.1 / 7.0 / 5.1 / 5.0 / 5.0 / 5.1 / 0.9 | 0.1 / 0.1 / 0.1 / 0.1 / 0.1 / 0.1 / 0.1 / 0.0 |
| capital gain distributions (E01100) | 1.6 / 1.7 / 1.8 / 1.8 / 1.8 / 1.8 / 1.8 / 1.5 | 3.6 / 3.9 / 3.8 / 3.5 / 3.5 / 3.5 / 3.5 / 3.4 | 6.8 / 6.8 / 8.1 / 6.5 / 6.5 / 6.5 / 6.5 / 5.8 | 4.3 / 3.7 / 7.6 / 4.7 / 4.7 / 4.7 / 4.7 / 4.1 | 1.1 / 1.2 / 3.6 / 2.1 / 2.1 / 2.1 / 2.1 / 2.0 | 2.9 / 3.0 / 3.2 / 2.9 / 2.9 / 2.9 / 2.9 / 2.7 |
| unemployment (E02300) | 3.8 / 3.7 / 3.8 / 3.8 / 3.8 / 3.8 / 3.8 / 3.3 | 4.7 / 4.7 / 4.8 / 4.9 / 4.9 / 4.9 / 4.9 / 4.6 | 3.1 / 2.1 / 4.4 / 3.5 / 3.5 / 3.5 / 3.5 / 3.6 | 1.0 / 0.7 / 5.3 / 1.8 / 1.8 / 1.8 / 1.8 / 2.0 | 0.3 / 0.1 / 6.6 / 0.5 / 0.4 / 0.4 / 0.5 / 1.3 | 4.1 / 3.9 / 4.3 / 4.2 / 4.2 / 4.2 / 4.2 / 3.8 |
| IRA deduction (E03150) | 0.7 / 0.6 / 0.7 / 0.8 / 0.8 / 0.8 / 0.8 / 0.8 | 2.7 / 2.7 / 2.5 / 2.5 / 2.5 / 2.5 / 2.5 / 2.2 | 3.0 / 3.3 / 4.3 / 3.1 / 3.1 / 3.1 / 3.1 / 3.1 | 2.7 / 2.5 / 7.5 / 3.3 / 3.3 / 3.3 / 3.3 / 3.5 | 2.4 / 2.3 / 10.7 / 3.2 / 3.1 / 3.1 / 3.2 / 3.3 | 1.8 / 1.7 / 1.8 / 1.7 / 1.7 / 1.7 / 1.7 / 1.6 |
| student loan interest (E03210) | 4.8 / 4.9 / 5.0 / 5.1 / 5.1 / 5.1 / 5.1 / 4.9 | 13.5 / 13.6 / 13.7 / 13.8 / 13.9 / 13.9 / 13.8 / 12.1 | 4.7 / 5.8 / 6.7 / 5.8 / 5.7 / 5.7 / 5.8 / 7.2 | 0.0 / 0.0 / 2.3 / 0.3 / 0.3 / 0.3 / 0.3 / 2.0 | 0.0 / 0.0 / 2.4 / 0.1 / 0.1 / 0.1 / 0.1 / 1.8 | 8.2 / 8.4 / 8.6 / 8.6 / 8.6 / 8.6 / 8.6 / 7.9 |
| self-employed health insurance (E03270) | 1.3 / 1.6 / 1.4 / 1.5 / 1.5 / 1.5 / 1.5 / 1.5 | 2.9 / 2.6 / 2.8 / 2.7 / 2.7 / 2.7 / 2.7 / 2.3 | 7.6 / 6.7 / 9.0 / 8.0 / 7.9 / 7.9 / 8.0 / 6.9 | 21.1 / 21.2 / 26.4 / 21.9 / 21.8 / 21.8 / 21.9 / 21.3 | 27.6 / 28.8 / 38.5 / 27.9 / 27.9 / 27.9 / 27.9 / 27.3 | 2.7 / 2.6 / 2.9 / 2.8 / 2.8 / 2.8 / 2.8 / 2.5 |
| medical (E17500) | 4.1 / 3.6 / 3.6 / 3.7 / 3.7 / 3.7 / 3.7 / 3.3 | 8.3 / 8.3 / 8.3 / 8.0 / 8.0 / 8.0 / 8.0 / 7.7 | 5.0 / 3.4 / 8.3 / 6.5 / 6.5 / 6.5 / 6.5 / 7.7 | 0.9 / 0.9 / 6.9 / 2.6 / 2.7 / 2.7 / 2.6 / 6.0 | 0.1 / 0.2 / 6.9 / 1.0 / 1.0 / 1.0 / 1.0 / 6.7 | 5.8 / 5.5 / 6.0 / 5.7 / 5.6 / 5.6 / 5.7 / 5.5 |
| state and local income/sales tax (E18400) | 6.8 / 5.8 / 7.1 / 7.3 / 7.3 / 7.3 / 7.3 / 8.2 | 41.0 / 40.8 / 40.9 / 41.1 / 41.1 / 41.1 / 41.1 / 39.2 | 84.4 / 85.8 / 83.8 / 82.7 / 82.5 / 82.5 / 82.7 / 79.8 | 78.5 / 77.5 / 78.8 / 78.3 / 78.4 / 78.4 / 78.3 / 82.5 | 81.6 / 80.2 / 80.5 / 81.5 / 81.3 / 81.3 / 81.5 / 80.6 | 28.2 / 27.5 / 28.3 / 28.4 / 28.3 / 28.3 / 28.4 / 27.5 |
| real estate tax (E18500) | 5.2 / 4.8 / 5.5 / 5.7 / 5.7 / 5.7 / 5.7 / 6.7 | 35.8 / 36.2 / 35.9 / 35.8 / 35.8 / 35.8 / 35.8 / 35.0 | 80.9 / 82.2 / 80.6 / 78.8 / 78.9 / 78.9 / 78.8 / 76.3 | 87.2 / 86.4 / 87.3 / 86.0 / 85.9 / 85.9 / 86.0 / 81.2 | 87.8 / 86.7 / 89.5 / 88.5 / 88.5 / 88.5 / 88.5 / 80.0 | 25.1 / 24.9 / 25.3 / 25.2 / 25.2 / 25.2 / 25.2 / 24.8 |
| interest paid (E19200) | 4.1 / 4.0 / 4.3 / 4.5 / 4.4 / 4.4 / 4.5 / 5.3 | 32.3 / 33.0 / 32.2 / 32.2 / 32.3 / 32.3 / 32.2 / 31.2 | 72.2 / 72.6 / 72.0 / 70.3 / 70.4 / 70.4 / 70.3 / 68.1 | 73.2 / 74.6 / 73.8 / 72.0 / 71.8 / 71.8 / 72.0 / 65.5 | 74.2 / 74.6 / 72.5 / 73.9 / 73.9 / 73.9 / 73.9 / 62.2 | 22.2 / 22.3 / 22.3 / 22.2 / 22.2 / 22.2 / 22.2 / 21.7 |
| cash charity (E19800) | 4.7 / 4.0 / 4.9 / 5.3 / 5.2 / 5.2 / 5.3 / 6.0 | 30.9 / 30.9 / 30.9 / 30.9 / 30.9 / 30.9 / 30.9 / 30.2 | 74.0 / 73.4 / 73.7 / 71.8 / 71.9 / 71.9 / 71.8 / 68.4 | 85.4 / 85.0 / 86.1 / 84.5 / 84.4 / 84.4 / 84.5 / 75.9 | 89.3 / 88.2 / 91.1 / 90.0 / 90.0 / 90.0 / 90.0 / 75.4 | 22.2 / 21.6 / 22.4 / 22.4 / 22.3 / 22.3 / 22.4 / 21.8 |
| noncash charity (E20100) | 2.7 / 2.6 / 2.9 / 3.1 / 3.1 / 3.1 / 3.1 / 3.6 | 21.3 / 21.1 / 21.1 / 21.1 / 21.1 / 21.1 / 21.1 / 20.8 | 52.3 / 51.6 / 52.7 / 50.2 / 50.2 / 50.2 / 50.2 / 48.2 | 48.3 / 47.7 / 52.0 / 48.1 / 47.8 / 47.8 / 48.1 / 45.4 | 42.7 / 42.9 / 45.0 / 43.3 / 43.1 / 43.1 / 43.3 / 36.9 | 15.1 / 14.7 / 15.2 / 15.0 / 15.0 / 15.0 / 15.0 / 14.8 |
| AGI remainder (AGI_REMAINDER) | 42.2 / 42.4 / 42.3 / 42.6 / 42.5 / 42.5 / 42.6 / 41.8 | 78.2 / 79.3 / 78.0 / 77.9 / 78.0 / 78.0 / 77.9 / 77.3 | 97.9 / 98.0 / 97.7 / 97.7 / 97.7 / 97.7 / 97.7 / 97.1 | 97.6 / 97.9 / 97.9 / 97.8 / 97.8 / 97.8 / 97.8 / 97.8 | 99.7 / 99.6 / 99.7 / 99.6 / 99.6 / 99.6 / 99.6 / 96.8 | 62.2 / 62.6 / 62.2 / 62.3 / 62.2 / 62.2 / 62.3 / 61.3 |

### Largest dollar misses, PUF validation (mean over seeds; generated minus real, $bn)

| Variable | Group | Real | Generated | Difference |
|---|---|---|---|---|
| total pensions (E01500) | 90-99 | 310 | 255 | -55 |
| wages (E00200) | 50-90 | 2,654 | 2,601 | -53 |
| total pensions (E01500) | 50-90 | 462 | 504 | +42 |
| taxable pensions (E01700) | 50-90 | 307 | 332 | +25 |
| taxable pensions (E01700) | 90-99 | 148 | 130 | -18 |
| wages (E00200) | bottom 50 | 810 | 827 | +17 |
| AGI remainder (AGI_REMAINDER) | bottom 50 | -165 | -181 | -16 |
| gross Social Security (E02400) | bottom 50 | 210 | 196 | -14 |
| Schedule E total (E02000) | 50-90 | 47 | 58 | +11 |
| wages (E00200) | 90-99 | 1,590 | 1,581 | -10 |
| wages (E00200) | top 0.1 | 211 | 220 | +10 |
| Schedule E total (E02000) | top 0.1 | 190 | 181 | -9 |
| total pensions (E01500) | bottom 50 | 122 | 131 | +8 |
| Schedule C (E00900) | 50-90 | 83 | 75 | -8 |
| partnership and S corp (E26270) | top 0.1 | 175 | 167 | -8 |

### Largest dollar misses, BL trees, PUF alone (probe BL, minimum leaf 20) (mean over seeds; generated minus real, $bn)

| Variable | Group | Real | Generated | Difference |
|---|---|---|---|---|
| long-term gains (P23250) | top 0.1 | 248 | 295 | +47 |
| long-term gains (P23250) | 99-99.9 | 111 | 126 | +15 |
| AGI remainder (AGI_REMAINDER) | top 0.1 | 7 | 21 | +14 |
| Schedule C (E00900) | 99-99.9 | 33 | 45 | +12 |
| ordinary dividends (E00600) | top 0.1 | 45 | 57 | +12 |
| Schedule C (E00900) | 90-99 | 82 | 92 | +11 |
| short-term gains (P22250) | bottom 50 | -7 | -16 | -10 |
| partnership and S corp (E26270) | top 0.1 | 175 | 166 | -9 |
| Schedule E total (E02000) | top 0.1 | 190 | 181 | -8 |
| taxable IRA (E01400) | 90-99 | 68 | 76 | +8 |
| qualified dividends (E00650) | top 0.1 | 36 | 44 | +8 |
| AGI remainder (AGI_REMAINDER) | 50-90 | 138 | 131 | -7 |
| ordinary dividends (E00600) | 99-99.9 | 39 | 46 | +7 |
| cash charity (E19800) | top 0.1 | 17 | 24 | +6 |
| ordinary dividends (E00600) | 90-99 | 55 | 61 | +6 |

### Largest dollar misses, Trees joint_leaf20 (mean over seeds; generated minus real, $bn)

| Variable | Group | Real | Generated | Difference |
|---|---|---|---|---|
| total pensions (E01500) | 50-90 | 462 | 442 | -20 |
| Schedule E total (E02000) | 99-99.9 | 205 | 221 | +16 |
| long-term gains (P23250) | top 0.1 | 248 | 260 | +12 |
| total pensions (E01500) | 99-99.9 | 42 | 54 | +12 |
| taxable pensions (E01700) | 90-99 | 148 | 159 | +11 |
| Schedule E total (E02000) | 90-99 | 153 | 162 | +9 |
| ordinary dividends (E00600) | 99-99.9 | 39 | 48 | +9 |
| taxable IRA (E01400) | 99-99.9 | 11 | 20 | +9 |
| rent and royalty income (E25850) | 90-99 | 28 | 36 | +8 |
| total pensions (E01500) | 90-99 | 310 | 319 | +8 |
| qualified dividends (E00650) | 99-99.9 | 31 | 40 | +8 |
| wages (E00200) | 50-90 | 2,654 | 2,646 | -8 |
| ordinary dividends (E00600) | 90-99 | 55 | 63 | +8 |
| AGI remainder (AGI_REMAINDER) | 50-90 | 138 | 130 | -8 |
| tax-exempt interest (E00400) | 90-99 | 15 | 22 | +8 |

### Largest dollar misses, Trees joint_leaf50 (mean over seeds; generated minus real, $bn)

| Variable | Group | Real | Generated | Difference |
|---|---|---|---|---|
| Schedule E total (E02000) | 99-99.9 | 205 | 220 | +15 |
| total pensions (E01500) | 50-90 | 462 | 449 | -13 |
| total pensions (E01500) | 99-99.9 | 42 | 55 | +12 |
| taxable pensions (E01700) | 90-99 | 148 | 159 | +11 |
| long-term gains (P23250) | top 0.1 | 248 | 258 | +11 |
| taxable IRA (E01400) | 99-99.9 | 11 | 21 | +10 |
| ordinary dividends (E00600) | 99-99.9 | 39 | 48 | +9 |
| qualified dividends (E00650) | 99-99.9 | 31 | 40 | +8 |
| rent and royalty income (E25850) | 90-99 | 28 | 36 | +8 |
| ordinary dividends (E00600) | 90-99 | 55 | 63 | +8 |
| tax-exempt interest (E00400) | 90-99 | 15 | 22 | +8 |
| AGI remainder (AGI_REMAINDER) | 50-90 | 138 | 130 | -7 |
| wages (E00200) | 99-99.9 | 455 | 462 | +7 |
| taxable pensions (E01700) | 99-99.9 | 11 | 18 | +7 |
| rent and royalty income (E25850) | 99-99.9 | 17 | 24 | +7 |

### Largest dollar misses, Trees joint_leaf50_uf (mean over seeds; generated minus real, $bn)

| Variable | Group | Real | Generated | Difference |
|---|---|---|---|---|
| Schedule E total (E02000) | 99-99.9 | 205 | 220 | +15 |
| total pensions (E01500) | 50-90 | 462 | 449 | -13 |
| total pensions (E01500) | 99-99.9 | 42 | 55 | +12 |
| taxable pensions (E01700) | 90-99 | 148 | 159 | +11 |
| long-term gains (P23250) | top 0.1 | 248 | 258 | +11 |
| taxable IRA (E01400) | 99-99.9 | 11 | 21 | +10 |
| ordinary dividends (E00600) | 99-99.9 | 39 | 48 | +9 |
| qualified dividends (E00650) | 99-99.9 | 31 | 40 | +8 |
| rent and royalty income (E25850) | 90-99 | 28 | 36 | +8 |
| ordinary dividends (E00600) | 90-99 | 55 | 63 | +8 |
| tax-exempt interest (E00400) | 90-99 | 15 | 22 | +8 |
| AGI remainder (AGI_REMAINDER) | 50-90 | 138 | 130 | -7 |
| wages (E00200) | 99-99.9 | 455 | 462 | +7 |
| taxable pensions (E01700) | 99-99.9 | 11 | 18 | +7 |
| rent and royalty income (E25850) | 99-99.9 | 17 | 24 | +7 |

### Largest dollar misses, Trees joint_leaf50_wv (mean over seeds; generated minus real, $bn)

| Variable | Group | Real | Generated | Difference |
|---|---|---|---|---|
| total pensions (E01500) | 50-90 | 462 | 442 | -20 |
| Schedule E total (E02000) | 99-99.9 | 205 | 221 | +16 |
| long-term gains (P23250) | top 0.1 | 248 | 260 | +12 |
| total pensions (E01500) | 99-99.9 | 42 | 54 | +12 |
| taxable pensions (E01700) | 90-99 | 148 | 159 | +11 |
| Schedule E total (E02000) | 90-99 | 153 | 162 | +9 |
| ordinary dividends (E00600) | 99-99.9 | 39 | 48 | +9 |
| taxable IRA (E01400) | 99-99.9 | 11 | 20 | +9 |
| rent and royalty income (E25850) | 90-99 | 28 | 36 | +8 |
| total pensions (E01500) | 90-99 | 310 | 319 | +8 |
| qualified dividends (E00650) | 99-99.9 | 31 | 40 | +8 |
| wages (E00200) | 50-90 | 2,654 | 2,646 | -8 |
| ordinary dividends (E00600) | 90-99 | 55 | 63 | +8 |
| AGI remainder (AGI_REMAINDER) | 50-90 | 138 | 130 | -8 |
| tax-exempt interest (E00400) | 90-99 | 15 | 22 | +8 |

### Largest dollar misses, Transformer (probe C, zero coding) (mean over seeds; generated minus real, $bn)

| Variable | Group | Real | Generated | Difference |
|---|---|---|---|---|
| total pensions (E01500) | 90-99 | 310 | 255 | -55 |
| long-term gains (P23250) | top 0.1 | 248 | 197 | -51 |
| wages (E00200) | 99-99.9 | 455 | 415 | -40 |
| Schedule E total (E02000) | bottom 50 | -33 | -68 | -36 |
| wages (E00200) | 90-99 | 1,590 | 1,562 | -28 |
| partnership and S corp (E26270) | bottom 50 | -31 | -59 | -28 |
| state and local income/sales tax (E18400) | top 0.1 | 50 | 23 | -28 |
| wages (E00200) | top 0.1 | 211 | 184 | -27 |
| gross Social Security (E02400) | bottom 50 | 210 | 189 | -21 |
| AGI remainder (AGI_REMAINDER) | 50-90 | 138 | 118 | -19 |
| AGI remainder (AGI_REMAINDER) | bottom 50 | -165 | -146 | +18 |
| gross Social Security (E02400) | 50-90 | 209 | 227 | +17 |
| Schedule E total (E02000) | top 0.1 | 190 | 207 | +17 |
| total pensions (E01500) | 50-90 | 462 | 446 | -15 |
| Schedule C (E00900) | 50-90 | 83 | 68 | -15 |

## 2. Joint relationships

### Filing status (1 single, 2 joint, 3 separate, 4 head of household), % of group; real / PUF validation / BL trees, PUF alone (probe BL, minimum leaf 20) / Trees joint_leaf20 / Trees joint_leaf50 / Trees joint_leaf50_uf / Trees joint_leaf50_wv / Transformer (probe C, zero coding)

| Group | 1 | 2 | 3 | 4 |
|---|---|---|---|---|
| bottom 50 | 63.4 / 63.6 / 63.5 / 63.0 / 63.0 / 63.0 / 63.0 / 62.6 | 15.2 / 14.3 / 15.2 / 15.6 / 15.6 / 15.6 / 15.6 / 15.5 | 1.8 / 1.7 / 1.8 / 1.8 / 1.8 / 1.8 / 1.8 / 1.7 | 19.6 / 20.4 / 19.5 / 19.6 / 19.6 / 19.6 / 19.6 / 20.2 |
| 50-90 | 35.9 / 35.3 / 35.6 / 36.0 / 35.9 / 35.9 / 36.0 / 37.4 | 50.1 / 49.9 / 50.4 / 50.1 / 50.1 / 50.1 / 50.1 / 50.0 | 2.4 / 2.7 / 2.4 / 2.4 / 2.4 / 2.4 / 2.4 / 2.1 | 11.6 / 12.1 / 11.6 / 11.5 / 11.6 / 11.6 / 11.5 / 10.6 |
| 90-99 | 12.0 / 12.6 / 12.9 / 12.6 / 12.5 / 12.5 / 12.6 / 13.3 | 84.3 / 84.3 / 83.0 / 83.5 / 83.6 / 83.6 / 83.5 / 82.4 | 0.8 / 0.6 / 1.0 / 1.0 / 1.0 / 1.0 / 1.0 / 0.8 | 3.0 / 2.4 / 3.1 / 2.8 / 3.0 / 3.0 / 2.8 / 3.5 |
| 99-99.9 | 11.1 / 11.3 / 14.0 / 11.8 / 11.6 / 11.6 / 11.8 / 15.0 | 85.5 / 85.4 / 80.8 / 84.5 / 84.7 / 84.7 / 84.5 / 80.2 | 1.3 / 1.0 / 1.6 / 1.3 / 1.3 / 1.3 / 1.3 / 1.0 | 2.1 / 2.3 / 3.7 / 2.4 / 2.4 / 2.4 / 2.4 / 3.8 |
| top 0.1 | 12.7 / 12.8 / 15.3 / 12.6 / 12.6 / 12.6 / 12.6 / 19.9 | 82.5 / 82.2 / 76.9 / 82.3 / 82.3 / 82.3 / 82.3 / 74.2 | 2.5 / 2.3 / 3.5 / 2.7 / 2.8 / 2.8 / 2.7 / 0.9 | 2.3 / 2.7 / 4.2 / 2.4 / 2.4 / 2.4 / 2.4 / 5.0 |

### Form of deduction (1 itemized, 2 standard, 3 none), % of group; real / PUF validation / BL trees, PUF alone (probe BL, minimum leaf 20) / Trees joint_leaf20 / Trees joint_leaf50 / Trees joint_leaf50_uf / Trees joint_leaf50_wv / Transformer (probe C, zero coding)

| Group | 1 | 2 | 3 |
|---|---|---|---|
| bottom 50 | 7.7 / 6.8 / 8.0 / 8.2 / 8.1 / 8.1 / 8.2 / 8.8 | 89.7 / 90.4 / 89.6 / 89.3 / 89.4 / 89.4 / 89.3 / 89.4 | 2.7 / 2.8 / 2.4 / 2.5 / 2.5 / 2.5 / 2.5 / 1.8 |
| 50-90 | 42.8 / 42.9 / 42.8 / 42.9 / 42.9 / 42.9 / 42.9 / 41.0 | 57.2 / 57.1 / 57.0 / 57.0 / 56.9 / 56.9 / 57.0 / 58.6 | 0.0 / 0.0 / 0.1 / 0.2 / 0.2 / 0.2 / 0.2 / 0.4 |
| 90-99 | 87.7 / 88.7 / 87.5 / 86.0 / 86.0 / 86.0 / 86.0 / 82.6 | 12.3 / 11.3 / 12.3 / 13.8 / 13.9 / 13.9 / 13.8 / 16.8 | 0.0 / 0.0 / 0.2 / 0.2 / 0.2 / 0.2 / 0.2 / 0.6 |
| 99-99.9 | 92.6 / 91.5 / 93.0 / 92.1 / 92.0 / 92.0 / 92.1 / 87.9 | 7.4 / 8.5 / 6.5 / 7.4 / 7.5 / 7.5 / 7.4 / 9.8 | 0.0 / 0.0 / 0.5 / 0.5 / 0.5 / 0.5 / 0.5 / 2.3 |
| top 0.1 | 92.8 / 91.4 / 93.9 / 94.0 / 93.9 / 93.9 / 94.0 / 86.2 | 7.2 / 8.6 / 5.1 / 5.5 / 5.4 / 5.4 / 5.5 / 8.1 | 0.0 / 0.0 / 1.0 / 0.5 / 0.6 / 0.6 / 0.5 / 5.6 |

### EITC children (0–3), % of group; real / PUF validation / BL trees, PUF alone (probe BL, minimum leaf 20) / Trees joint_leaf20 / Trees joint_leaf50 / Trees joint_leaf50_uf / Trees joint_leaf50_wv / Transformer (probe C, zero coding)

| Group | 0 | 1 | 2 | 3 |
|---|---|---|---|---|
| bottom 50 | 76.2 / 75.2 / 76.8 / 76.8 / 76.9 / 76.9 / 76.8 / 77.8 | 12.4 / 13.0 / 12.1 / 12.1 / 12.1 / 12.1 / 12.1 / 11.4 | 8.0 / 8.3 / 7.8 / 7.8 / 7.8 / 7.8 / 7.8 / 7.5 | 3.4 / 3.5 / 3.3 / 3.3 / 3.3 / 3.3 / 3.3 / 3.3 |
| 50-90 | 95.4 / 95.5 / 94.8 / 94.7 / 94.7 / 94.7 / 94.7 / 94.4 | 1.3 / 1.2 / 1.7 / 1.8 / 1.8 / 1.8 / 1.8 / 2.2 | 1.9 / 1.7 / 2.1 / 2.1 / 2.1 / 2.1 / 2.1 / 2.1 | 1.4 / 1.6 / 1.4 / 1.4 / 1.4 / 1.4 / 1.4 / 1.4 |
| 90-99 | 100.0 / 100.0 / 99.5 / 99.8 / 99.8 / 99.8 / 99.8 / 99.3 | 0.0 / 0.0 / 0.2 / 0.1 / 0.1 / 0.1 / 0.1 / 0.3 | 0.0 / 0.0 / 0.2 / 0.1 / 0.1 / 0.1 / 0.1 / 0.2 | 0.0 / 0.0 / 0.1 / 0.1 / 0.1 / 0.1 / 0.1 / 0.2 |
| 99-99.9 | 100.0 / 100.0 / 99.2 / 99.9 / 99.9 / 99.9 / 99.9 / 99.0 | 0.0 / 0.0 / 0.4 / 0.0 / 0.0 / 0.0 / 0.0 / 0.3 | 0.0 / 0.0 / 0.2 / 0.0 / 0.0 / 0.0 / 0.0 / 0.3 | 0.0 / 0.0 / 0.2 / 0.0 / 0.0 / 0.0 / 0.0 / 0.4 |
| top 0.1 | 100.0 / 100.0 / 98.4 / 99.9 / 99.9 / 99.9 / 99.9 / 98.5 | 0.0 / 0.0 / 0.5 / 0.0 / 0.0 / 0.0 / 0.0 / 0.4 | 0.0 / 0.0 / 0.8 / 0.0 / 0.0 / 0.0 / 0.0 / 0.4 | 0.0 / 0.0 / 0.3 / 0.1 / 0.0 / 0.0 / 0.1 / 0.7 |

### Child-tax-credit children (0, 1, 2, 3+), % of group; real / PUF validation / BL trees, PUF alone (probe BL, minimum leaf 20) / Trees joint_leaf20 / Trees joint_leaf50 / Trees joint_leaf50_uf / Trees joint_leaf50_wv / Transformer (probe C, zero coding)

| Group | 0 | 1 | 2 | 3 |
|---|---|---|---|---|
| bottom 50 | 76.2 / 75.9 / 76.3 / 76.1 / 76.1 / 76.1 / 76.1 / 75.3 | 12.9 / 13.2 / 12.8 / 12.9 / 12.9 / 12.9 / 12.9 / 13.4 | 7.7 / 7.8 / 7.7 / 7.8 / 7.8 / 7.8 / 7.8 / 8.0 | 3.2 / 3.1 / 3.2 / 3.2 / 3.2 / 3.2 / 3.2 / 3.4 |
| 50-90 | 71.3 / 71.6 / 71.3 / 71.3 / 71.3 / 71.3 / 71.3 / 72.4 | 14.0 / 13.7 / 14.0 / 14.0 / 14.1 / 14.1 / 14.0 / 13.1 | 9.8 / 10.0 / 9.9 / 10.0 / 9.9 / 9.9 / 10.0 / 9.8 | 4.9 / 4.8 / 4.8 / 4.7 / 4.7 / 4.7 / 4.7 / 4.7 |
| 90-99 | 64.3 / 65.4 / 64.8 / 64.8 / 64.6 / 64.6 / 64.8 / 63.8 | 14.4 / 15.6 / 14.4 / 14.1 / 14.2 / 14.2 / 14.1 / 15.0 | 15.6 / 14.1 / 14.9 / 15.4 / 15.4 / 15.4 / 15.4 / 14.9 | 5.7 / 5.0 / 5.9 / 5.7 / 5.8 / 5.8 / 5.7 / 6.4 |
| 99-99.9 | 61.1 / 60.9 / 63.7 / 64.3 / 64.4 / 64.4 / 64.3 / 67.1 | 13.5 / 13.1 / 15.0 / 13.0 / 12.9 / 12.9 / 13.0 / 13.5 | 17.0 / 19.3 / 14.8 / 15.3 / 15.3 / 15.3 / 15.3 / 12.6 | 8.4 / 6.7 / 6.5 / 7.3 / 7.4 / 7.4 / 7.3 / 6.9 |
| top 0.1 | 64.8 / 64.9 / 67.1 / 66.8 / 66.8 / 66.8 / 66.8 / 70.7 | 11.7 / 12.5 / 14.7 / 12.0 / 12.0 / 12.0 / 12.0 / 12.0 | 14.2 / 14.0 / 12.0 / 12.7 / 12.8 / 12.8 / 12.7 / 10.6 | 9.3 / 8.6 / 6.2 / 8.5 / 8.5 / 8.5 / 8.5 / 6.7 |

### Age range of primary filer (codes 1–6, youngest to oldest; real records: observed ones only, which is almost none above the 99th percentile), % of group; real / PUF validation / BL trees, PUF alone (probe BL, minimum leaf 20) / Trees joint_leaf20 / Trees joint_leaf50 / Trees joint_leaf50_uf / Trees joint_leaf50_wv / Transformer (probe C, zero coding)

| Group | 1 | 2 | 3 | 4 | 5 | 6 |
|---|---|---|---|---|---|---|
| bottom 50 | 21.2 / 21.8 / 21.2 / 21.1 / 21.1 / 21.1 / 21.1 / 21.0 | 28.8 / 28.2 / 28.7 / 28.3 / 28.4 / 28.4 / 28.3 / 29.4 | 13.4 / 14.0 / 13.3 / 13.6 / 13.6 / 13.6 / 13.6 / 13.8 | 11.3 / 11.6 / 11.4 / 11.5 / 11.5 / 11.5 / 11.5 / 11.2 | 10.4 / 10.0 / 10.5 / 10.7 / 10.7 / 10.7 / 10.7 / 9.9 | 14.9 / 14.5 / 14.9 / 14.7 / 14.7 / 14.7 / 14.7 / 14.7 |
| 50-90 | 4.3 / 4.2 / 4.5 / 4.4 / 4.4 / 4.4 / 4.4 / 5.0 | 18.6 / 18.5 / 18.5 / 18.4 / 18.4 / 18.4 / 18.4 / 18.7 | 20.4 / 19.9 / 20.5 / 20.6 / 20.5 / 20.5 / 20.6 / 19.5 | 20.4 / 20.3 / 20.5 / 20.6 / 20.6 / 20.6 / 20.6 / 20.2 | 19.2 / 19.1 / 19.1 / 19.0 / 19.0 / 19.0 / 19.0 / 18.4 | 17.2 / 18.1 / 16.8 / 17.1 / 17.1 / 17.1 / 17.1 / 18.2 |
| 90-99 | 0.2 / 0.2 / 0.4 / 0.3 / 0.3 / 0.3 / 0.3 / 0.6 | 8.1 / 8.4 / 8.5 / 8.0 / 8.1 / 8.1 / 8.0 / 7.5 | 21.9 / 21.5 / 21.0 / 21.2 / 21.1 / 21.1 / 21.2 / 22.1 | 27.7 / 27.4 / 27.3 / 26.8 / 26.8 / 26.8 / 26.8 / 26.1 | 23.6 / 26.1 / 24.6 / 24.5 / 24.4 / 24.4 / 24.5 / 25.3 | 18.4 / 16.4 / 18.2 / 19.2 / 19.4 / 19.4 / 19.2 / 18.4 |
| 99-99.9 | 0.0 / 0.0 / 1.6 / 0.1 / 0.2 / 0.2 / 0.1 / 0.7 | 0.0 / 0.0 / 6.8 / 1.8 / 1.9 / 1.9 / 1.8 / 4.6 | 0.0 / 0.0 / 13.6 / 4.8 / 5.1 / 5.1 / 4.8 / 18.4 | 0.0 / 0.0 / 20.3 / 11.7 / 11.1 / 11.1 / 11.7 / 24.7 | 0.0 / 0.0 / 28.7 / 19.6 / 19.8 / 19.8 / 19.6 / 29.1 | 100.0 / 0.0 / 28.9 / 61.9 / 61.9 / 61.9 / 61.9 / 22.6 |
| top 0.1 | 0.0 / 0.0 / 5.5 / 0.0 / 0.0 / 0.0 / 0.0 / 1.3 | 0.0 / 0.0 / 6.9 / 0.0 / 0.0 / 0.0 / 0.0 / 5.6 | 0.0 / 0.0 / 12.4 / 0.0 / 16.5 / 16.5 / 0.0 / 16.4 | 0.0 / 0.0 / 16.6 / 0.0 / 0.0 / 0.0 / 0.0 / 22.7 | 0.0 / 0.0 / 26.2 / 0.0 / 0.0 / 0.0 / 0.0 / 26.9 | 0.0 / 0.0 / 32.4 / 100.0 / 83.5 / 83.5 / 100.0 / 27.1 |

### Share holding each main item (% of group); real / PUF validation / BL trees, PUF alone (probe BL, minimum leaf 20) / Trees joint_leaf20 / Trees joint_leaf50 / Trees joint_leaf50_uf / Trees joint_leaf50_wv / Transformer (probe C, zero coding)

| Item | bottom 50 | 50-90 | 90-99 | 99-99.9 | top 0.1 |
|---|---|---|---|---|---|
| wages | 78.3 / 78.2 / 78.2 / 78.3 / 78.2 / 78.2 / 78.3 / 78.6 | 87.1 / 86.5 / 87.3 / 87.4 / 87.3 / 87.3 / 87.4 / 87.0 | 88.5 / 89.5 / 88.6 / 88.7 / 88.6 / 88.6 / 88.7 / 89.9 | 85.9 / 86.4 / 85.0 / 84.6 / 84.6 / 84.6 / 84.6 / 85.9 | 82.7 / 82.6 / 82.9 / 82.4 / 82.5 / 82.5 / 82.4 / 80.4 |
| interest | 14.7 / 14.7 / 15.3 / 15.4 / 15.4 / 15.4 / 15.4 / 15.2 | 35.3 / 34.0 / 35.2 / 35.1 / 35.2 / 35.2 / 35.1 / 35.3 | 67.1 / 68.0 / 67.2 / 67.4 / 67.3 / 67.3 / 67.4 / 64.3 | 91.1 / 92.4 / 90.2 / 90.1 / 90.3 / 90.3 / 90.1 / 83.7 | 98.3 / 98.1 / 98.4 / 98.0 / 98.0 / 98.0 / 98.0 / 89.0 |
| dividends | 8.8 / 8.6 / 9.2 / 9.6 / 9.5 / 9.5 / 9.6 / 9.0 | 21.5 / 21.8 / 21.5 / 22.0 / 22.0 / 22.0 / 22.0 / 21.4 | 51.1 / 52.2 / 51.3 / 52.8 / 52.9 / 52.9 / 52.8 / 47.1 | 81.2 / 79.9 / 80.1 / 82.5 / 82.7 / 82.7 / 82.5 / 77.3 | 91.4 / 91.3 / 90.5 / 93.7 / 93.8 / 93.8 / 93.7 / 87.6 |
| capital gains | 6.8 / 6.7 / 7.2 / 7.4 / 7.3 / 7.3 / 7.4 / 6.2 | 16.6 / 17.4 / 17.0 / 17.1 / 17.1 / 17.1 / 17.1 / 15.8 | 44.9 / 45.8 / 47.7 / 47.1 / 47.2 / 47.2 / 47.1 / 39.3 | 79.9 / 81.8 / 83.8 / 83.2 / 83.3 / 83.3 / 83.2 / 73.6 | 93.2 / 92.9 / 95.2 / 96.2 / 96.2 / 96.2 / 96.2 / 87.0 |
| Schedule C | 16.2 / 16.5 / 16.0 / 16.0 / 16.0 / 16.0 / 16.0 / 14.6 | 15.3 / 14.0 / 15.6 / 15.4 / 15.4 / 15.4 / 15.4 / 14.4 | 21.7 / 22.2 / 23.2 / 21.9 / 22.0 / 22.0 / 21.9 / 22.0 | 25.7 / 25.7 / 29.4 / 28.1 / 27.9 / 27.9 / 28.1 / 33.2 | 25.6 / 26.0 / 31.8 / 27.8 / 27.8 / 27.8 / 27.8 / 38.4 |
| partnership and S corp | 2.2 / 2.2 / 2.2 / 2.2 / 2.2 / 2.2 / 2.2 / 2.2 | 5.7 / 6.5 / 5.6 / 5.8 / 5.8 / 5.8 / 5.8 / 5.4 | 20.7 / 20.5 / 20.5 / 20.4 / 20.3 / 20.3 / 20.4 / 18.8 | 59.1 / 59.8 / 58.2 / 58.4 / 58.1 / 58.1 / 58.4 / 54.0 | 81.9 / 81.5 / 81.2 / 81.8 / 81.6 / 81.6 / 81.8 / 74.3 |
| pensions | 13.8 / 14.2 / 13.9 / 14.0 / 13.9 / 13.9 / 14.0 / 13.5 | 25.9 / 26.7 / 25.8 / 26.0 / 26.0 / 26.0 / 26.0 / 26.3 | 32.8 / 30.5 / 33.2 / 33.5 / 33.7 / 33.7 / 33.5 / 32.5 | 25.5 / 24.5 / 27.2 / 28.4 / 28.5 / 28.5 / 28.4 / 28.9 | 24.7 / 23.8 / 25.6 / 26.3 / 26.3 / 26.3 / 26.3 / 25.7 |
| Social Security | 18.1 / 17.7 / 18.0 / 17.8 / 17.7 / 17.7 / 17.8 / 16.5 | 19.3 / 20.6 / 19.1 / 19.0 / 19.1 / 19.1 / 19.0 / 20.4 | 19.4 / 17.6 / 20.3 / 20.3 / 20.4 / 20.4 / 20.3 / 20.0 | 17.4 / 17.1 / 20.2 / 20.2 / 20.4 / 20.4 / 20.2 / 21.5 | 18.2 / 15.4 / 24.2 / 20.5 / 20.3 / 20.3 / 20.5 / 23.8 |

### Co-holding: the 12 largest gaps, PUF validation (% of group holding both)

| Pair | Group | Real | Generated |
|---|---|---|---|
| interest & Social Security | top 0.1 | 18.1 | 15.3 |
| dividends & Social Security | top 0.1 | 17.1 | 14.5 |
| partnership and S corp & Social Security | top 0.1 | 16.0 | 13.4 |
| capital gains & Social Security | top 0.1 | 17.3 | 14.8 |
| interest & capital gains | 99-99.9 | 75.1 | 77.5 |
| wages & Social Security | top 0.1 | 12.2 | 9.9 |
| wages & capital gains | 99-99.9 | 67.7 | 69.8 |
| pensions & Social Security | 90-99 | 14.2 | 12.2 |
| wages & interest | 99-99.9 | 77.6 | 79.5 |
| wages & interest | 90-99 | 57.3 | 58.9 |
| capital gains & partnership and S corp | 99-99.9 | 48.8 | 50.4 |
| pensions & Social Security | top 0.1 | 9.8 | 8.2 |

### Co-holding: the 12 largest gaps, BL trees, PUF alone (probe BL, minimum leaf 20) (% of group holding both)

| Pair | Group | Real | Generated |
|---|---|---|---|
| capital gains & Schedule C | top 0.1 | 23.8 | 30.2 |
| capital gains & Social Security | top 0.1 | 17.3 | 23.4 |
| interest & Schedule C | top 0.1 | 25.1 | 31.1 |
| interest & Social Security | top 0.1 | 18.1 | 23.9 |
| dividends & Schedule C | top 0.1 | 23.3 | 28.6 |
| dividends & Social Security | top 0.1 | 17.1 | 22.2 |
| wages & Social Security | top 0.1 | 12.2 | 17.2 |
| partnership and S corp & Social Security | top 0.1 | 16.0 | 21.0 |
| wages & Schedule C | top 0.1 | 20.7 | 25.6 |
| Schedule C & partnership and S corp | top 0.1 | 21.4 | 26.0 |
| capital gains & Schedule C | 99-99.9 | 20.5 | 24.6 |
| Schedule C & Social Security | top 0.1 | 5.2 | 9.1 |

### Co-holding: the 12 largest gaps, Trees joint_leaf20 (% of group holding both)

| Pair | Group | Real | Generated |
|---|---|---|---|
| capital gains & Schedule C | top 0.1 | 23.8 | 26.8 |
| dividends & capital gains | top 0.1 | 88.7 | 91.6 |
| capital gains & pensions | 99-99.9 | 21.5 | 24.3 |
| dividends & Schedule C | top 0.1 | 23.3 | 26.0 |
| capital gains & Social Security | top 0.1 | 17.3 | 20.0 |
| capital gains & Social Security | 99-99.9 | 15.2 | 17.8 |
| dividends & Social Security | top 0.1 | 17.1 | 19.7 |
| wages & capital gains | top 0.1 | 76.6 | 79.1 |
| capital gains & Schedule C | 99-99.9 | 20.5 | 23.0 |
| interest & Social Security | 99-99.9 | 16.6 | 18.9 |
| interest & capital gains | top 0.1 | 92.3 | 94.6 |
| pensions & Social Security | 99-99.9 | 10.0 | 12.2 |

### Co-holding: the 12 largest gaps, Trees joint_leaf50 (% of group holding both)

| Pair | Group | Real | Generated |
|---|---|---|---|
| dividends & capital gains | top 0.1 | 88.7 | 91.7 |
| capital gains & Schedule C | top 0.1 | 23.8 | 26.7 |
| capital gains & pensions | 99-99.9 | 21.5 | 24.4 |
| capital gains & Social Security | 99-99.9 | 15.2 | 18.0 |
| dividends & Schedule C | top 0.1 | 23.3 | 26.0 |
| wages & capital gains | top 0.1 | 76.6 | 79.2 |
| capital gains & Schedule C | 99-99.9 | 20.5 | 23.0 |
| dividends & Social Security | 99-99.9 | 15.3 | 17.7 |
| interest & Social Security | 99-99.9 | 16.6 | 19.0 |
| capital gains & Social Security | top 0.1 | 17.3 | 19.7 |
| dividends & Social Security | top 0.1 | 17.1 | 19.4 |
| pensions & Social Security | 99-99.9 | 10.0 | 12.3 |

### Co-holding: the 12 largest gaps, Trees joint_leaf50_uf (% of group holding both)

| Pair | Group | Real | Generated |
|---|---|---|---|
| dividends & capital gains | top 0.1 | 88.7 | 91.7 |
| capital gains & Schedule C | top 0.1 | 23.8 | 26.7 |
| capital gains & pensions | 99-99.9 | 21.5 | 24.4 |
| capital gains & Social Security | 99-99.9 | 15.2 | 18.0 |
| dividends & Schedule C | top 0.1 | 23.3 | 26.0 |
| wages & capital gains | top 0.1 | 76.6 | 79.2 |
| capital gains & Schedule C | 99-99.9 | 20.5 | 23.0 |
| dividends & Social Security | 99-99.9 | 15.3 | 17.7 |
| interest & Social Security | 99-99.9 | 16.6 | 19.0 |
| capital gains & Social Security | top 0.1 | 17.3 | 19.7 |
| dividends & Social Security | top 0.1 | 17.1 | 19.4 |
| pensions & Social Security | 99-99.9 | 10.0 | 12.3 |

### Co-holding: the 12 largest gaps, Trees joint_leaf50_wv (% of group holding both)

| Pair | Group | Real | Generated |
|---|---|---|---|
| capital gains & Schedule C | top 0.1 | 23.8 | 26.8 |
| dividends & capital gains | top 0.1 | 88.7 | 91.6 |
| capital gains & pensions | 99-99.9 | 21.5 | 24.3 |
| dividends & Schedule C | top 0.1 | 23.3 | 26.0 |
| capital gains & Social Security | top 0.1 | 17.3 | 20.0 |
| capital gains & Social Security | 99-99.9 | 15.2 | 17.8 |
| dividends & Social Security | top 0.1 | 17.1 | 19.7 |
| wages & capital gains | top 0.1 | 76.6 | 79.1 |
| capital gains & Schedule C | 99-99.9 | 20.5 | 23.0 |
| interest & Social Security | 99-99.9 | 16.6 | 18.9 |
| interest & capital gains | top 0.1 | 92.3 | 94.6 |
| pensions & Social Security | 99-99.9 | 10.0 | 12.2 |

### Co-holding: the 12 largest gaps, Transformer (probe C, zero coding) (% of group holding both)

| Pair | Group | Real | Generated |
|---|---|---|---|
| interest & partnership and S corp | top 0.1 | 81.1 | 67.4 |
| interest & capital gains | top 0.1 | 92.3 | 79.8 |
| capital gains & partnership and S corp | top 0.1 | 77.0 | 65.5 |
| interest & dividends | top 0.1 | 90.6 | 80.4 |
| dividends & Schedule C | top 0.1 | 23.3 | 33.2 |
| wages & interest | top 0.1 | 81.3 | 71.5 |
| interest & partnership and S corp | 99-99.9 | 56.0 | 46.3 |
| dividends & partnership and S corp | top 0.1 | 75.5 | 65.9 |
| interest & capital gains | 99-99.9 | 75.1 | 65.7 |
| wages & Schedule C | top 0.1 | 20.7 | 29.9 |
| capital gains & Schedule C | top 0.1 | 23.8 | 32.9 |
| interest & Schedule C | top 0.1 | 25.1 | 34.0 |

## 3. Rules that hold on every real training record: weighted share of returns breaking each (%)

51 rules were found on the real training records. 'Validation' is the real held-out records, which shows how strict each rule is. Listed: the 30 rules most often broken by any model.

| Rule | Validation | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| E01700 <= E01500 | 0.000 | 0.000 | 5.784 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E00650 <= E00600 | 0.000 | 0.000 | 2.018 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| combination of MARS, DSI, XTOT (capped 6), N24 (capped 4), EIC, FDED never seen on a real training record | 0.020 | 0.020 | 0.078 | 0.088 | 0.091 | 0.091 | 0.088 | 0.620 |
| EIC > 0 => AGI <= 82,000 (highest AGI with EITC children on a real record) | 0.000 | 0.000 | 0.125 | 0.098 | 0.093 | 0.093 | 0.098 | 0.210 |
| MARS = 3 => E03210 = 0 | 0.000 | 0.000 | 0.067 | 0.075 | 0.073 | 0.073 | 0.075 | 0.144 |
| FDED = 1 and no itemized amount (real share shown, not a rule) | 0.041 | 0.041 | 0.137 | 0.115 | 0.113 | 0.113 | 0.115 | 0.100 |
| DSI = 1 => E03210 = 0 | 0.000 | 0.000 | 0.028 | 0.035 | 0.040 | 0.040 | 0.035 | 0.037 |
| FDED = 2 => E20100 = 0 | 0.000 | 0.000 | 0.022 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| N24 <= XTOT | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.021 |
| EIC = 3 => E00400 = 0 | 0.000 | 0.000 | 0.013 | 0.011 | 0.009 | 0.009 | 0.011 | 0.005 |
| FDED = 2 => E19200 = 0 | 0.000 | 0.000 | 0.005 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| EIC = 2 => E26390 = 0 | 0.000 | 0.000 | 0.003 | 0.004 | 0.003 | 0.003 | 0.004 | 0.003 |
| FDED = 2 => E17500 = 0 | 0.000 | 0.000 | 0.003 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| FDED = 3 => E17500 = 0 | 0.000 | 0.000 | 0.003 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| FDED = 3 => E20100 = 0 | 0.000 | 0.000 | 0.002 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| EIC = 3 => E26390 = 0 | 0.000 | 0.000 | 0.002 | 0.002 | 0.002 | 0.002 | 0.002 | 0.002 |
| FDED = 3 => E19800 = 0 | 0.000 | 0.000 | 0.002 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| FDED = 2 => E19800 = 0 | 0.000 | 0.000 | 0.001 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| FDED = 3 => E19200 = 0 | 0.000 | 0.000 | 0.001 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| EIC = 3 => E26400 = 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| EIC = 2 => E26400 = 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E00200 >= 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E00600 >= 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E00650 >= 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E00300 >= 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E01500 >= 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E01700 >= 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E01400 >= 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E02400 >= 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |
| E25850 >= 0 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 | 0.000 |

### Orderings broken by more than 5% (the larger amount exceeds the smaller by over 5% plus $1), % of returns

| Rule | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|
| E01700 <= E01500 | 0.00 | 1.23 [1.20–1.25] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] |
| E00650 <= E00600 | 0.00 | 0.75 [0.75–0.76] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] |

### Any rule broken, by AGI group (%)

| Group | Validation | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| bottom 50 | 0.00 | 0.00 | 5.10 [5.04–5.16] | 0.20 | 0.21 [0.20–0.23] | 0.21 [0.20–0.23] | 0.20 | 1.00 [0.89–1.05] |
| 50-90 | 0.05 | 0.05 | 9.56 [9.55–9.58] | 0.44 | 0.41 [0.39–0.44] | 0.41 [0.39–0.44] | 0.44 | 1.02 [0.96–1.11] |
| 90-99 | 0.00 | 0.00 | 14.78 [14.67–14.88] | 0.29 | 0.28 [0.28–0.29] | 0.28 [0.28–0.29] | 0.29 | 0.83 [0.69–0.92] |
| 99-99.9 | 0.00 | 0.00 | 14.37 [14.26–14.48] | 0.12 | 0.14 [0.12–0.17] | 0.14 [0.12–0.17] | 0.12 | 1.32 [1.21–1.51] |
| top 0.1 | 0.00 | 0.00 | 16.18 [16.16–16.19] | 0.11 | 0.10 [0.09–0.11] | 0.10 [0.09–0.11] | 0.11 | 1.83 [1.74–1.97] |

## 4. Simplified 2015 federal income tax by AGI group

### Net income tax after credits ($bn)

| Group | Real | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| bottom 50 | -37 | -39 | -35 [-35–-35] | -35 | -35 [-35–-35] | -35 [-35–-35] | -35 | -30 [-31–-29] |
| 50-90 | 325 | 324 | 323 [322–324] | 321 | 321 [321–321] | 321 [321–321] | 321 | 321 [318–322] |
| 90-99 | 373 | 367 | 380 [380–380] | 385 | 384 [382–385] | 384 [382–385] | 385 | 368 [361–372] |
| 99-99.9 | 230 | 232 | 242 [240–245] | 248 | 248 [248–249] | 248 [248–249] | 248 | 231 [225–238] |
| top 0.1 | 204 | 200 | 219 [204–234] | 215 | 214 [213–215] | 214 [213–215] | 215 | 204 [189–216] |
| all | 1,095 | 1,084 | 1,129 [1,112–1,146] | 1,134 | 1,132 [1,130–1,134] | 1,132 [1,130–1,134] | 1,134 | 1,093 [1,077–1,109] |

### Average tax rate: net income tax / AGI (%)

| Group | Real | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| bottom 50 | -4.48 | -4.63 | -4.28 [-4.32–-4.24] | -4.32 | -4.31 [-4.32–-4.29] | -4.31 [-4.32–-4.29] | -4.32 | -3.93 [-4.10–-3.74] |
| 50-90 | 9.52 | 9.52 | 9.49 [9.49–9.49] | 9.50 | 9.50 [9.50–9.50] | 9.50 [9.50–9.50] | 9.50 | 9.56 [9.53–9.58] |
| 90-99 | 16.60 | 16.71 | 16.68 [16.67–16.69] | 16.79 | 16.77 [16.75–16.79] | 16.77 [16.75–16.79] | 16.79 | 16.90 [16.87–16.97] |
| 99-99.9 | 26.35 | 26.47 | 26.18 [26.12–26.24] | 26.41 | 26.45 [26.41–26.49] | 26.45 [26.41–26.49] | 26.41 | 27.20 [27.13–27.28] |
| top 0.1 | 27.99 | 27.91 | 27.23 [26.81–27.65] | 28.15 | 28.15 [28.14–28.15] | 28.15 [28.14–28.15] | 28.15 | 30.68 [30.44–30.90] |
| all | 13.53 | 13.50 | 13.71 [13.60–13.81] | 13.85 | 13.83 [13.82–13.85] | 13.83 [13.82–13.85] | 13.85 | 13.98 [13.79–14.14] |

### Income tax before credits ($bn)

| Group | Real | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| bottom 50 | 36 | 37 | 36 [36–36] | 36 | 36 [36–36] | 36 [36–36] | 36 | 37 [37–37] |
| 50-90 | 349 | 348 | 347 [346–348] | 345 | 345 [345–345] | 345 [345–345] | 345 | 343 [340–345] |
| 90-99 | 373 | 367 | 380 [380–381] | 386 | 384 [382–386] | 384 [382–386] | 386 | 368 [361–373] |
| 99-99.9 | 230 | 232 | 242 [240–245] | 248 | 248 [248–249] | 248 [248–249] | 248 | 231 [225–238] |
| top 0.1 | 204 | 200 | 219 [204–234] | 215 | 214 [213–215] | 214 [213–215] | 215 | 204 [189–216] |
| all | 1,193 | 1,184 | 1,225 [1,208–1,241] | 1,229 | 1,227 [1,224–1,229] | 1,227 [1,224–1,229] | 1,229 | 1,183 [1,168–1,200] |

### Earned income credit ($bn)

| Group | Real | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| bottom 50 | 53.4 | 55.2 | 51.7 [51.6–51.7] | 51.6 | 51.5 [51.4–51.6] | 51.5 [51.4–51.6] | 51.6 | 47.9 [46.4–48.7] |
| 50-90 | 2.6 | 2.5 | 2.3 [2.3–2.3] | 2.2 | 2.2 [2.2–2.2] | 2.2 [2.2–2.2] | 2.2 | 1.7 [1.6–1.8] |
| 90-99 | 0.0 | 0.0 | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] |
| 99-99.9 | 0.0 | 0.0 | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] |
| top 0.1 | 0.0 | 0.0 | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] |
| all | 55.9 | 57.7 | 54.0 [53.9–54.0] | 53.8 | 53.7 [53.5–53.8] | 53.7 [53.5–53.8] | 53.8 | 49.6 [48.2–50.4] |

### Child tax credit, including the refundable part ($bn)

| Group | Real | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| bottom 50 | 19.6 | 20.2 | 19.4 [19.4–19.4] | 19.5 | 19.5 [19.5–19.5] | 19.5 [19.5–19.5] | 19.5 | 19.2 [18.8–19.5] |
| 50-90 | 21.7 | 21.6 | 21.6 [21.5–21.7] | 21.4 | 21.4 [21.3–21.4] | 21.4 [21.3–21.4] | 21.4 | 20.8 [20.4–21.0] |
| 90-99 | 0.4 | 0.3 | 0.4 [0.4–0.4] | 0.4 | 0.4 [0.4–0.4] | 0.4 [0.4–0.4] | 0.4 | 0.4 [0.4–0.4] |
| 99-99.9 | 0.0 | 0.0 | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] |
| top 0.1 | 0.0 | 0.0 | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] | 0.0 [0.0–0.0] | 0.0 | 0.0 [0.0–0.0] |
| all | 41.7 | 42.1 | 41.4 [41.3–41.4] | 41.3 | 41.2 [41.1–41.3] | 41.2 [41.1–41.3] | 41.3 | 40.4 [40.2–40.7] |

### Share of total net income tax (%)

| Group | Real | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| bottom 50 | -3.4 | -3.6 | -3.1 [-3.2–-3.1] | -3.1 | -3.1 [-3.1–-3.1] | -3.1 [-3.1–-3.1] | -3.1 | -2.8 [-2.9–-2.6] |
| 50-90 | 29.7 | 29.9 | 28.6 [28.1–29.1] | 28.3 | 28.4 [28.3–28.4] | 28.4 [28.3–28.4] | 28.3 | 29.3 [28.7–29.9] |
| 90-99 | 34.0 | 33.9 | 33.7 [33.2–34.1] | 34.0 | 33.9 [33.8–34.0] | 33.9 [33.8–34.0] | 34.0 | 33.7 [33.1–34.6] |
| 99-99.9 | 21.0 | 21.4 | 21.5 [21.4–21.5] | 21.9 | 21.9 [21.9–22.0] | 21.9 [21.9–22.0] | 21.9 | 21.1 [20.9–21.4] |
| top 0.1 | 18.7 | 18.4 | 19.4 [18.4–20.4] | 19.0 | 18.9 [18.9–19.0] | 18.9 [18.9–19.0] | 19.0 | 18.7 [17.6–19.4] |

### Share of returns with tax before credits (%)

| Group | Real | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| bottom 50 | 53.2 | 54.5 | 53.0 [53.0–53.0] | 52.7 | 52.7 [52.7–52.8] | 52.7 [52.7–52.8] | 52.7 | 52.8 [52.8–53.0] |
| 50-90 | 99.2 | 99.2 | 99.2 [99.2–99.2] | 99.1 | 99.1 [99.1–99.1] | 99.1 [99.1–99.1] | 99.1 | 98.9 [98.8–98.9] |
| 90-99 | 99.9 | 99.9 | 99.9 [99.9–99.9] | 99.8 | 99.8 [99.8–99.9] | 99.8 [99.8–99.9] | 99.8 | 99.8 [99.7–99.8] |
| 99-99.9 | 99.9 | 99.9 | 99.9 [99.9–99.9] | 99.9 | 99.9 [99.9–99.9] | 99.9 [99.9–99.9] | 99.9 | 99.6 [99.4–99.7] |
| top 0.1 | 99.9 | 99.9 | 99.9 [99.8–99.9] | 99.9 | 99.9 [99.9–99.9] | 99.9 [99.9–99.9] | 99.9 | 99.9 [99.7–100.0] |
| all | 76.3 | 76.8 | 76.2 [76.2–76.2] | 75.9 | 76.0 [75.9–76.0] | 76.0 [75.9–76.0] | 75.9 | 75.7 [75.5–75.8] |

### AGI ($bn)

| Group | Real | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| bottom 50 | 830 | 840 | 825 [817–832] | 816 | 816 [816–817] | 816 [816–817] | 816 | 775 [764–795] |
| 50-90 | 3,415 | 3,398 | 3,404 [3,394–3,414] | 3,380 | 3,382 [3,380–3,384] | 3,382 [3,380–3,384] | 3,380 | 3,355 [3,324–3,380] |
| 90-99 | 2,245 | 2,198 | 2,278 [2,278–2,279] | 2,295 | 2,286 [2,278–2,295] | 2,286 [2,278–2,295] | 2,295 | 2,175 [2,139–2,207] |
| 99-99.9 | 873 | 877 | 925 [918–933] | 939 | 939 [938–939] | 939 [938–939] | 939 | 849 [829–871] |
| top 0.1 | 731 | 716 | 806 [739–873] | 764 | 761 [758–764] | 761 [758–764] | 764 | 665 [616–708] |
| all | 8,094 | 8,029 | 8,238 [8,180–8,297] | 8,193 | 8,184 [8,175–8,193] | 8,184 [8,175–8,193] | 8,193 | 7,819 [7,804–7,845] |

## 5. Classifier: generated against real

Area under the ROC curve: the chance that the classifier scores a randomly chosen generated record as more likely generated than a randomly chosen real one (0.5 = cannot tell them apart, 1 = always can). Weighted, classes balanced, five-fold cross-validated.

| | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|
| All returns (one model) | 0.501 | 0.540 [0.540–0.540] | 0.562 | 0.562 [0.561–0.562] | 0.562 [0.561–0.562] | 0.562 | 0.814 [0.812–0.816] |
| bottom 50, model trained within the group | 0.514 | 0.528 [0.528–0.529] | 0.540 | 0.542 [0.540–0.543] | 0.542 [0.540–0.543] | 0.540 | 0.773 [0.770–0.776] |
| 50-90, model trained within the group | 0.508 | 0.539 [0.539–0.539] | 0.556 | 0.555 [0.553–0.556] | 0.555 [0.553–0.556] | 0.556 | 0.873 [0.871–0.875] |
| 90-99, model trained within the group | 0.506 | 0.595 [0.591–0.599] | 0.589 | 0.586 [0.583–0.589] | 0.586 [0.583–0.589] | 0.589 | 0.912 [0.908–0.920] |
| 99-99.9, model trained within the group | 0.499 | 0.624 [0.614–0.634] | 0.639 | 0.642 [0.639–0.645] | 0.642 [0.639–0.645] | 0.639 | 0.847 [0.845–0.848] |
| top 0.1, model trained within the group | 0.507 | 0.583 [0.581–0.584] | 0.646 | 0.648 [0.646–0.651] | 0.648 [0.646–0.651] | 0.646 | 0.802 [0.797–0.808] |

### The same classifier on the rank-band tokens (the model's own codes) instead of decoded dollars

| | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|
| All returns (one model) | 0.502 | 0.530 [0.526–0.534] | — | — | — | — | — |
| bottom 50, model trained within the group | 0.503 | 0.516 [0.516–0.517] | — | — | — | — | — |
| 50-90, model trained within the group | 0.507 | 0.528 [0.526–0.531] | — | — | — | — | — |
| 90-99, model trained within the group | 0.514 | 0.589 [0.585–0.592] | — | — | — | — | — |
| 99-99.9, model trained within the group | 0.504 | 0.630 [0.620–0.641] | — | — | — | — | — |
| top 0.1, model trained within the group | 0.502 | 0.598 [0.582–0.614] | — | — | — | — | — |

- PUF validation, tokens, all returns, leans on: partnership and S corp (E26270) 0.007; short-term gains (P22250) 0.006; gross Social Security (E02400) 0.004; tax-exempt interest (E00400) 0.004; AGI remainder (AGI_REMAINDER) 0.003; taxable pensions (E01700) 0.002; Schedule E total (E02000) 0.001; noncash charity (E20100) 0.001
- BL trees, PUF alone (probe BL, minimum leaf 20), tokens, all returns, leans on: MARS (MARS) 0.009; taxable interest (E00300) 0.008; short-term gains (P22250) 0.006; ordinary dividends (E00600) 0.006; medical (E17500) 0.006; interest paid (E19200) 0.005; state and local income/sales tax (E18400) 0.005; qualified dividends (E00650) 0.004

**PUF validation (PUF validation): variables the all-returns classifier leans on most** (drop in area under the curve when the variable is shuffled): wages (E00200) 0.010; AGI (E00100) 0.008; cash charity (E19800) 0.005; state and local income/sales tax (E18400) 0.003; real estate tax (E18500) 0.003; tax-exempt interest (E00400) 0.002; short-term gains (P22250) 0.002; taxable interest (E00300) 0.002

- within bottom 50: AGI (E00100) 0.021; gross Social Security (E02400) 0.011; wages (E00200) 0.010; taxable interest (E00300) 0.007; state and local income/sales tax (E18400) 0.006; MARS (MARS) 0.006
- within 50-90: cash charity (E19800) 0.007; AGI (E00100) 0.006; Schedule F (E02100) 0.006; rent and royalty income (E25850) 0.006; total pensions (E01500) 0.005; wages (E00200) 0.005
- within 90-99: partnership and S corp (E26270) 0.008; state and local income/sales tax (E18400) 0.007; AGI (E00100) 0.006; capital gain distributions (E01100) 0.005; qualified dividends (E00650) 0.005; rent and royalty loss (E25860) 0.004
- within 99-99.9: N24 (N24) 0.010; total pensions (E01500) 0.005; qualified dividends (E00650) 0.005; cash charity (E19800) 0.004; state and local income/sales tax (E18400) 0.003; noncash charity (E20100) 0.003
- within top 0.1: wages (E00200) 0.018; interest paid (E19200) 0.008; tax-exempt interest (E00400) 0.005; state and local income/sales tax (E18400) 0.005; rent and royalty loss (E25860) 0.005; AGI remainder (AGI_REMAINDER) 0.004

**BL trees, PUF alone (probe BL, minimum leaf 20) (BL trees, PUF alone (probe BL, minimum leaf 20) seed 1): variables the all-returns classifier leans on most** (drop in area under the curve when the variable is shuffled): AGI (E00100) 0.021; taxable interest (E00300) 0.010; state and local income/sales tax (E18400) 0.010; medical (E17500) 0.010; interest paid (E19200) 0.009; FDED (FDED) 0.008; AGI remainder (AGI_REMAINDER) 0.008; MARS (MARS) 0.007

- within bottom 50: AGI (E00100) 0.018; medical (E17500) 0.010; FDED (FDED) 0.010; AGI remainder (AGI_REMAINDER) 0.008; EIC (EIC) 0.005; long-term gains (P23250) 0.004
- within 50-90: medical (E17500) 0.012; EIC (EIC) 0.012; interest paid (E19200) 0.011; AGI (E00100) 0.010; XTOT (XTOT) 0.008; gross Social Security (E02400) 0.007
- within 90-99: medical (E17500) 0.025; AGI (E00100) 0.024; student loan interest (E03210) 0.017; capital gain distributions (E01100) 0.010; wages (E00200) 0.009; short-term gains (P22250) 0.008
- within 99-99.9: IRA deduction (E03150) 0.019; medical (E17500) 0.018; rent and royalty income (E25850) 0.017; interest paid (E19200) 0.017; unemployment (E02300) 0.016; taxable interest (E00300) 0.014
- within top 0.1: rent and royalty loss (E25860) 0.077; partnership and S corp (E26270) 0.019; wages (E00200) 0.015; Schedule E total (E02000) 0.014; ordinary dividends (E00600) 0.014; IRA deduction (E03150) 0.014

**Trees joint_leaf20 (Trees joint_leaf20 seed 1): variables the all-returns classifier leans on most** (drop in area under the curve when the variable is shuffled): AGI (E00100) 0.034; ordinary dividends (E00600) 0.016; long-term gains (P23250) 0.015; Schedule E total (E02000) 0.014; cash charity (E19800) 0.011; AGI remainder (AGI_REMAINDER) 0.011; short-term gains (P22250) 0.011; qualified dividends (E00650) 0.010

- within bottom 50: AGI (E00100) 0.022; FDED (FDED) 0.018; AGI remainder (AGI_REMAINDER) 0.014; N24 (N24) 0.005; Schedule E total (E02000) 0.005; partnership and S corp (E26270) 0.004
- within 50-90: AGI (E00100) 0.025; gross Social Security (E02400) 0.020; AGI remainder (AGI_REMAINDER) 0.019; EIC (EIC) 0.014; XTOT (XTOT) 0.010; state and local income/sales tax (E18400) 0.010
- within 90-99: Schedule E total (E02000) 0.032; AGI (E00100) 0.028; short-term gains (P22250) 0.019; interest paid (E19200) 0.014; medical (E17500) 0.012; state and local income/sales tax (E18400) 0.011
- within 99-99.9: Schedule E total (E02000) 0.038; short-term gains (P22250) 0.027; state and local income/sales tax (E18400) 0.020; taxable interest (E00300) 0.014; partnership and S corp (E26270) 0.013; ordinary dividends (E00600) 0.012
- within top 0.1: short-term gains (P22250) 0.041; ordinary dividends (E00600) 0.029; taxable interest (E00300) 0.024; Schedule E total (E02000) 0.023; rent and royalty income (E25850) 0.022; qualified dividends (E00650) 0.013

**Trees joint_leaf50 (Trees joint_leaf50 seed 1): variables the all-returns classifier leans on most** (drop in area under the curve when the variable is shuffled): AGI (E00100) 0.034; ordinary dividends (E00600) 0.016; long-term gains (P23250) 0.015; Schedule E total (E02000) 0.014; cash charity (E19800) 0.011; AGI remainder (AGI_REMAINDER) 0.011; short-term gains (P22250) 0.011; qualified dividends (E00650) 0.010

- within bottom 50: AGI (E00100) 0.022; FDED (FDED) 0.018; AGI remainder (AGI_REMAINDER) 0.014; N24 (N24) 0.005; Schedule E total (E02000) 0.005; partnership and S corp (E26270) 0.004
- within 50-90: AGI (E00100) 0.025; gross Social Security (E02400) 0.020; AGI remainder (AGI_REMAINDER) 0.019; EIC (EIC) 0.014; XTOT (XTOT) 0.010; state and local income/sales tax (E18400) 0.010
- within 90-99: Schedule E total (E02000) 0.032; AGI (E00100) 0.028; short-term gains (P22250) 0.019; interest paid (E19200) 0.014; medical (E17500) 0.012; state and local income/sales tax (E18400) 0.011
- within 99-99.9: Schedule E total (E02000) 0.038; short-term gains (P22250) 0.027; state and local income/sales tax (E18400) 0.020; taxable interest (E00300) 0.014; partnership and S corp (E26270) 0.013; ordinary dividends (E00600) 0.012
- within top 0.1: short-term gains (P22250) 0.041; ordinary dividends (E00600) 0.029; taxable interest (E00300) 0.024; Schedule E total (E02000) 0.023; rent and royalty income (E25850) 0.022; qualified dividends (E00650) 0.013

**Trees joint_leaf50_uf (Trees joint_leaf50_uf seed 1): variables the all-returns classifier leans on most** (drop in area under the curve when the variable is shuffled): AGI (E00100) 0.034; ordinary dividends (E00600) 0.016; long-term gains (P23250) 0.015; Schedule E total (E02000) 0.014; cash charity (E19800) 0.011; AGI remainder (AGI_REMAINDER) 0.011; short-term gains (P22250) 0.011; qualified dividends (E00650) 0.010

- within bottom 50: AGI (E00100) 0.022; FDED (FDED) 0.018; AGI remainder (AGI_REMAINDER) 0.014; N24 (N24) 0.005; Schedule E total (E02000) 0.005; partnership and S corp (E26270) 0.004
- within 50-90: AGI (E00100) 0.025; gross Social Security (E02400) 0.020; AGI remainder (AGI_REMAINDER) 0.019; EIC (EIC) 0.014; XTOT (XTOT) 0.010; state and local income/sales tax (E18400) 0.010
- within 90-99: Schedule E total (E02000) 0.032; AGI (E00100) 0.028; short-term gains (P22250) 0.019; interest paid (E19200) 0.014; medical (E17500) 0.012; state and local income/sales tax (E18400) 0.011
- within 99-99.9: Schedule E total (E02000) 0.038; short-term gains (P22250) 0.027; state and local income/sales tax (E18400) 0.020; taxable interest (E00300) 0.014; partnership and S corp (E26270) 0.013; ordinary dividends (E00600) 0.012
- within top 0.1: short-term gains (P22250) 0.041; ordinary dividends (E00600) 0.029; taxable interest (E00300) 0.024; Schedule E total (E02000) 0.023; rent and royalty income (E25850) 0.022; qualified dividends (E00650) 0.013

**Trees joint_leaf50_wv (Trees joint_leaf50_wv seed 1): variables the all-returns classifier leans on most** (drop in area under the curve when the variable is shuffled): AGI (E00100) 0.034; ordinary dividends (E00600) 0.016; long-term gains (P23250) 0.015; Schedule E total (E02000) 0.014; cash charity (E19800) 0.011; AGI remainder (AGI_REMAINDER) 0.011; short-term gains (P22250) 0.011; qualified dividends (E00650) 0.010

- within bottom 50: AGI (E00100) 0.022; FDED (FDED) 0.018; AGI remainder (AGI_REMAINDER) 0.014; N24 (N24) 0.005; Schedule E total (E02000) 0.005; partnership and S corp (E26270) 0.004
- within 50-90: AGI (E00100) 0.025; gross Social Security (E02400) 0.020; AGI remainder (AGI_REMAINDER) 0.019; EIC (EIC) 0.014; XTOT (XTOT) 0.010; state and local income/sales tax (E18400) 0.010
- within 90-99: Schedule E total (E02000) 0.032; AGI (E00100) 0.028; short-term gains (P22250) 0.019; interest paid (E19200) 0.014; medical (E17500) 0.012; state and local income/sales tax (E18400) 0.011
- within 99-99.9: Schedule E total (E02000) 0.038; short-term gains (P22250) 0.027; state and local income/sales tax (E18400) 0.020; taxable interest (E00300) 0.014; partnership and S corp (E26270) 0.013; ordinary dividends (E00600) 0.012
- within top 0.1: short-term gains (P22250) 0.041; ordinary dividends (E00600) 0.029; taxable interest (E00300) 0.024; Schedule E total (E02000) 0.023; rent and royalty income (E25850) 0.022; qualified dividends (E00650) 0.013

**Transformer (probe C, zero coding) (Transformer (probe C, zero coding) seed 1): variables the all-returns classifier leans on most** (drop in area under the curve when the variable is shuffled): AGI remainder (AGI_REMAINDER) 0.120; AGI (E00100) 0.111; taxable interest (E00300) 0.089; gross Social Security (E02400) 0.048; noncash charity (E20100) 0.040; state and local income/sales tax (E18400) 0.038; qualified dividends (E00650) 0.029; FDED (FDED) 0.025

- within bottom 50: AGI remainder (AGI_REMAINDER) 0.140; AGI (E00100) 0.121; FDED (FDED) 0.048; taxable interest (E00300) 0.038; gross Social Security (E02400) 0.037; wages (E00200) 0.032
- within 50-90: AGI remainder (AGI_REMAINDER) 0.141; taxable interest (E00300) 0.099; gross Social Security (E02400) 0.095; noncash charity (E20100) 0.047; AGI (E00100) 0.042; student loan interest (E03210) 0.022
- within 90-99: taxable interest (E00300) 0.148; noncash charity (E20100) 0.082; state and local income/sales tax (E18400) 0.041; AGI (E00100) 0.034; gross Social Security (E02400) 0.025; wages (E00200) 0.022
- within 99-99.9: state and local income/sales tax (E18400) 0.136; AGI (E00100) 0.032; AGI remainder (AGI_REMAINDER) 0.028; cash charity (E19800) 0.028; wages (E00200) 0.027; noncash charity (E20100) 0.020
- within top 0.1: state and local income/sales tax (E18400) 0.185; cash charity (E19800) 0.048; IRA deduction (E03150) 0.023; AGI remainder (AGI_REMAINDER) 0.022; qualified dividends (E00650) 0.021; partnership and S corp (E26270) 0.019

## 6. Copying

Distance: Euclidean distance over 37 variables, each replaced by its weighted rank among the real training records (0 to 1); the demographic codes are left out because the real file lacks them for 42% of records.

| | Real validation | PUF validation | BL trees, PUF alone (probe BL, minimum leaf 20) | Trees joint_leaf20 | Trees joint_leaf50 | Trees joint_leaf50_uf | Trees joint_leaf50_wv | Transformer (probe C, zero coding) |
|---|---|---|---|---|---|---|---|---|
| Median distance to nearest training record | 0.0531 | 0.0531 | 0.0552 [0.0550–0.0555] | 0.0600 | 0.0588 [0.0575–0.0600] | 0.0588 [0.0575–0.0600] | 0.0600 | 0.0841 [0.0820–0.0870] |
| 5th percentile of that distance | 0.0000 | 0.0000 | 0.0000 [0.0000–0.0000] | 0.0000 | 0.0000 [0.0000–0.0000] | 0.0000 [0.0000–0.0000] | 0.0000 | 0.0000 [0.0000–0.0000] |
| Share nearer than the real records' 1st percentile (%) | 1.00 | 0.00 | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] |
| Share nearer than the real records' 5th percentile (%) | 5.00 | 0.00 | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] |
| Share at distance zero (%) | 26.65 | 26.65 | 28.91 [28.91–28.91] | 28.26 | 28.52 [28.26–28.78] | 28.52 [28.26–28.78] | 28.26 | 25.65 [25.31–26.13] |
| Top 0.1%: median distance | 0.4443 | 0.4443 | 0.5618 [0.5579–0.5657] | 0.5101 | 0.5098 [0.5095–0.5101] | 0.5098 [0.5095–0.5101] | 0.5101 | 0.5665 [0.5568–0.5819] |
| Top 0.1%: share at distance zero (%) | pending | 0.08 | 0.00 [0.00–0.00] | 0.00 | 0.00 [0.00–0.00] | 0.00 [0.00–0.00] | 0.00 | 0.10 [0.00–0.16] |
| Identical token pattern to a training record (%) | 34.22 | 34.22 | 35.31 [35.29–35.33] | — | — | — | — | — |
| Same, top 0.1% (%) | pending | 0.06 | 0.00 [0.00–0.00] | — | — | — | — | — |

### Copying by AGI group: share at distance zero (%) and median distance; real validation / PUF validation / BL trees, PUF alone (probe BL, minimum leaf 20) / Trees joint_leaf20 / Trees joint_leaf50 / Trees joint_leaf50_uf / Trees joint_leaf50_wv / Transformer (probe C, zero coding)

| Group | Share at distance zero | Median distance |
|---|---|---|
| bottom 50 | 43.01 / 43.01 / 46.20 / 45.26 / 45.32 / 45.32 / 45.26 / 40.46 | 0.010 / 0.010 / 0.002 / 0.011 / 0.011 / 0.011 / 0.011 / 0.009 |
| 50-90 | 12.45 / 12.45 / 14.46 / 14.37 / 14.56 / 14.56 / 14.37 / 12.97 | 0.162 / 0.162 / 0.125 / 0.173 / 0.173 / 0.173 / 0.173 / 0.146 |
| 90-99 | 0.13 / 0.13 / 0.34 / 0.17 / 0.19 / 0.19 / 0.17 / 0.26 | 0.417 / 0.417 / 0.318 / 0.486 / 0.484 / 0.484 / 0.486 / 0.316 |
| 99-99.9 | 0.04 / 0.04 / 0.00 / 0.00 / 0.00 / 0.00 / 0.00 / 0.13 | 0.470 / 0.470 / 0.536 / 0.518 / 0.520 / 0.520 / 0.518 / 0.540 |
| top 0.1 | 0.06 / 0.06 / 0.00 / 0.00 / 0.00 / 0.00 / 0.00 / 0.10 | 0.444 / 0.444 / 0.562 / 0.510 / 0.510 / 0.510 / 0.510 / 0.566 |

- PUF validation identical token pattern by group (%): bottom 50 53.5; 50-90 18.0; 90-99 0.7; 99-99.9 0.1; top 0.1 0.1
- BL trees, PUF alone (probe BL, minimum leaf 20) identical token pattern by group (%): bottom 50 55.4; 50-90 18.9; 90-99 0.7; 99-99.9 0.0; top 0.1 0.0


