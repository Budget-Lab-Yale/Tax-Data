---
title: "Minimal example: PUF against SCF by dollar bracket, the detection table"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Minimal example: PUF against SCF by dollar bracket, the detection table

For each variable, the first table gives the weighted share of units in each dollar bracket in the PUF and in the SCF (tax year 2015, shares within source, capital gains on the absolute amount) and the ratio SCF/PUF; the second gives that ratio as a reporting rate taking the PUF as the truth, by bracket and cumulatively (share at or above the bracket lower bound, SCF over PUF), with a star on the first bracket above zero where the ratio is 0.9 or more; the third gives the weighted mean amount within each bracket in each source. Brackets include the lower bound and exclude the upper one, "1-100" covers amounts above 0 and under 100, and the record counts are unweighted.

Produced by `research/rebuild/minimal/01_detection_table.py` from `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/stacked.parquet`.

## Wages

### Share of units by bracket (wages)

| Bracket ($) | PUF share | SCF share | SCF / PUF | PUF records | SCF records |
|---|---:|---:|---:|---:|---:|
| 0 | 17.14% | 30.45% | 1.776 | 42,144 | 10,399 |
| 1-100 | 0.13% | 0.03% | 0.215 | 276 | 10 |
| 100-500 | 0.48% | 0.25% | 0.520 | 870 | 64 |
| 500-2,000 | 2.01% | 0.75% | 0.374 | 2,923 | 233 |
| 2,000-10,000 | 11.06% | 5.20% | 0.470 | 14,101 | 1,536 |
| 10,000-50,000 | 39.32% | 30.81% | 0.784 | 49,344 | 9,260 |
| 50,000-200,000 | 27.15% | 28.41% | 1.046 | 49,327 | 9,553 |
| 200,000+ | 2.71% | 4.10% | 1.514 | 48,707 | 3,322 |

### Reporting rate taking the PUF as the truth

| Bracket ($) | By bracket: SCF / PUF | Cumulative: PUF share at or above | Cumulative: SCF share at or above | Cumulative: SCF / PUF |
|---|---:|---:|---:|---:|
| 0 | 1.776 | 100.00% | 100.00% | 1.000 |
| 1-100 | 0.215 | 82.86% | 69.55% | 0.839 |
| 100-500 | 0.520 | 82.73% | 69.53% | 0.840 |
| 500-2,000 | 0.374 | 82.25% | 69.28% | 0.842 |
| 2,000-10,000 | 0.470 | 80.24% | 68.52% | 0.854 |
| 10,000-50,000 | 0.784 | 69.18% | 63.33% | 0.915 * |
| 50,000-200,000 | 1.046 * | 29.86% | 32.51% | 1.089 |
| 200,000+ | 1.514 | 2.71% | 4.10% | 1.514 |

\* first bracket above zero, counting up, where the ratio is 0.9 or more.

### Weighted mean amount within each bracket ($)

| Bracket ($) | PUF mean | SCF mean | SCF / PUF |
|---|---:|---:|---:|
| 0 | 0 | 0 |  |
| 1-100 | 46 | 1 | 0.022 |
| 100-500 | 285 | 318 | 1.113 |
| 500-2,000 | 1,272 | 1,138 | 0.895 |
| 2,000-10,000 | 5,898 | 5,971 | 1.012 |
| 10,000-50,000 | 27,530 | 28,641 | 1.040 |
| 50,000-200,000 | 91,962 | 92,165 | 1.002 |
| 200,000+ | 402,329 | 477,342 | 1.186 |

## Interest

### Share of units by bracket (interest)

| Bracket ($) | PUF share | SCF share | SCF / PUF | PUF records | SCF records |
|---|---:|---:|---:|---:|---:|
| 0 | 71.67% | 87.35% | 1.219 | 88,144 | 26,138 |
| 1-100 | 14.75% | 3.97% | 0.269 | 32,772 | 1,371 |
| 100-500 | 7.13% | 2.89% | 0.405 | 22,697 | 1,254 |
| 500-2,000 | 3.46% | 2.65% | 0.765 | 17,013 | 1,378 |
| 2,000-10,000 | 2.14% | 1.98% | 0.923 | 17,540 | 1,438 |
| 10,000-50,000 | 0.69% | 0.97% | 1.410 | 13,769 | 1,241 |
| 50,000-200,000 | 0.13% | 0.14% | 1.096 | 8,286 | 765 |
| 200,000+ | 0.03% | 0.05% | 1.649 | 7,471 | 792 |

### Reporting rate taking the PUF as the truth

| Bracket ($) | By bracket: SCF / PUF | Cumulative: PUF share at or above | Cumulative: SCF share at or above | Cumulative: SCF / PUF |
|---|---:|---:|---:|---:|
| 0 | 1.219 | 100.00% | 100.00% | 1.000 |
| 1-100 | 0.269 | 28.33% | 12.65% | 0.446 |
| 100-500 | 0.405 | 13.58% | 8.68% | 0.639 |
| 500-2,000 | 0.765 | 6.46% | 5.79% | 0.897 |
| 2,000-10,000 | 0.923 * | 2.99% | 3.14% | 1.050 * |
| 10,000-50,000 | 1.410 | 0.85% | 1.16% | 1.371 |
| 50,000-200,000 | 1.096 | 0.16% | 0.19% | 1.195 |
| 200,000+ | 1.649 | 0.03% | 0.05% | 1.649 |

\* first bracket above zero, counting up, where the ratio is 0.9 or more.

### Weighted mean amount within each bracket ($)

| Bracket ($) | PUF mean | SCF mean | SCF / PUF |
|---|---:|---:|---:|
| 0 | 0 | 0 |  |
| 1-100 | 33 | 33 | 1.023 |
| 100-500 | 228 | 218 | 0.955 |
| 500-2,000 | 1,011 | 928 | 0.918 |
| 2,000-10,000 | 4,447 | 4,187 | 0.941 |
| 10,000-50,000 | 20,006 | 18,266 | 0.913 |
| 50,000-200,000 | 89,432 | 92,903 | 1.039 |
| 200,000+ | 667,117 | 480,648 | 0.720 |

## Dividends

### Share of units by bracket (dividends)

| Bracket ($) | PUF share | SCF share | SCF / PUF | PUF records | SCF records |
|---|---:|---:|---:|---:|---:|
| 0 | 81.63% | 87.97% | 1.078 | 111,053 | 26,427 |
| 1-100 | 3.72% | 1.37% | 0.368 | 8,390 | 448 |
| 100-500 | 4.05% | 2.23% | 0.552 | 10,141 | 860 |
| 500-2,000 | 3.99% | 2.58% | 0.645 | 12,275 | 1,058 |
| 2,000-10,000 | 4.12% | 2.91% | 0.705 | 18,644 | 1,459 |
| 10,000-50,000 | 2.00% | 2.21% | 1.109 | 20,311 | 1,601 |
| 50,000-200,000 | 0.41% | 0.62% | 1.506 | 13,472 | 1,175 |
| 200,000+ | 0.08% | 0.11% | 1.397 | 13,406 | 1,349 |

### Reporting rate taking the PUF as the truth

| Bracket ($) | By bracket: SCF / PUF | Cumulative: PUF share at or above | Cumulative: SCF share at or above | Cumulative: SCF / PUF |
|---|---:|---:|---:|---:|
| 0 | 1.078 | 100.00% | 100.00% | 1.000 |
| 1-100 | 0.368 | 18.37% | 12.03% | 0.655 |
| 100-500 | 0.552 | 14.65% | 10.66% | 0.728 |
| 500-2,000 | 0.645 | 10.61% | 8.43% | 0.795 |
| 2,000-10,000 | 0.705 | 6.61% | 5.85% | 0.885 |
| 10,000-50,000 | 1.109 * | 2.49% | 2.94% | 1.184 * |
| 50,000-200,000 | 1.506 | 0.49% | 0.73% | 1.488 |
| 200,000+ | 1.397 | 0.08% | 0.11% | 1.397 |

\* first bracket above zero, counting up, where the ratio is 0.9 or more.

### Weighted mean amount within each bracket ($)

| Bracket ($) | PUF mean | SCF mean | SCF / PUF |
|---|---:|---:|---:|
| 0 | 0 | 0 |  |
| 1-100 | 38 | 41 | 1.093 |
| 100-500 | 250 | 228 | 0.914 |
| 500-2,000 | 1,095 | 1,048 | 0.956 |
| 2,000-10,000 | 4,756 | 4,609 | 0.969 |
| 10,000-50,000 | 21,042 | 21,892 | 1.040 |
| 50,000-200,000 | 88,060 | 87,684 | 0.996 |
| 200,000+ | 690,914 | 524,986 | 0.760 |

## Capital gains

### Share of units by bracket (capital gains, absolute amount)

| Bracket ($) | PUF share | SCF share | SCF / PUF | PUF records | SCF records |
|---|---:|---:|---:|---:|---:|
| 0 | 86.67% | 91.63% | 1.057 | 114,947 | 28,168 |
| 1-100 | 0.82% | 0.16% | 0.196 | 1,958 | 65 |
| 100-500 | 1.32% | 0.74% | 0.561 | 3,191 | 249 |
| 500-2,000 | 2.21% | 1.39% | 0.629 | 6,782 | 551 |
| 2,000-10,000 | 6.25% | 2.76% | 0.441 | 36,940 | 1,596 |
| 10,000-50,000 | 1.86% | 2.05% | 1.099 | 11,159 | 1,253 |
| 50,000-200,000 | 0.62% | 0.83% | 1.327 | 9,241 | 999 |
| 200,000+ | 0.25% | 0.44% | 1.811 | 23,474 | 1,496 |

Share with a negative amount (a net loss): PUF 5.58%, SCF 1.77%, SCF / PUF 0.316.

### Reporting rate taking the PUF as the truth

| Bracket ($) | By bracket: SCF / PUF | Cumulative: PUF share at or above | Cumulative: SCF share at or above | Cumulative: SCF / PUF |
|---|---:|---:|---:|---:|
| 0 | 1.057 | 100.00% | 100.00% | 1.000 |
| 1-100 | 0.196 | 13.33% | 8.37% | 0.628 |
| 100-500 | 0.561 | 12.51% | 8.21% | 0.656 |
| 500-2,000 | 0.629 | 11.20% | 7.47% | 0.667 |
| 2,000-10,000 | 0.441 | 8.98% | 6.08% | 0.676 |
| 10,000-50,000 | 1.099 * | 2.73% | 3.32% | 1.215 * |
| 50,000-200,000 | 1.327 | 0.87% | 1.27% | 1.464 |
| 200,000+ | 1.811 | 0.25% | 0.44% | 1.811 |

\* first bracket above zero, counting up, where the ratio is 0.9 or more.

### Weighted mean amount within each bracket ($, absolute amount)

| Bracket ($) | PUF mean | SCF mean | SCF / PUF |
|---|---:|---:|---:|
| 0 | 0 | 0 |  |
| 1-100 | 34 | 41 | 1.189 |
| 100-500 | 273 | 256 | 0.937 |
| 500-2,000 | 1,149 | 1,075 | 0.936 |
| 2,000-10,000 | 3,749 | 4,086 | 1.090 |
| 10,000-50,000 | 22,380 | 22,838 | 1.020 |
| 50,000-200,000 | 93,611 | 88,189 | 0.942 |
| 200,000+ | 1,249,777 | 866,115 | 0.693 |
