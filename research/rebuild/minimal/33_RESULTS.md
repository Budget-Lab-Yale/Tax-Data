---
title: "Step 33: the whole chain with eleven more PUF items — results"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 33: the whole chain with eleven more PUF items

The question (John, 18 October): how does the whole method behave when
more variables are added? Step 31's model has married, dependents, wages,
interest, dividends, capital gains in four pieces, other income (the
PUF-owned remainder of AGI) and net worth (SCF-owned). Step 33 adds eleven
PUF items, three of which the SCF also measures, and reruns everything:
table, pretraining with the cell method, generation, 2019 dollars, the
tilt. Jobs and how to resume: `33_STATUS.md`. Report tables written by the
last job: `$R/step33/report.md` (`$R` = `/nfs/roberts/scratch/pi_nrs36/jar335/minimal`).

## Summary

- **The PUF side scales.** With eleven more items (business income,
  retirement income, tax-exempt interest, and eight PUF-only lines of the
  1040), the generated file reproduces the PUF closely: for every item but
  the rare ones, share nonzero within about one point and the 50th and 90th
  percentiles within about 5%. The items step 31 already had fit about as
  before (dividends and capital gains means a few points lower). AGI is
  computed from the parts and matches to within 2% at the 50th, 90th and
  99th percentiles.
- **The IRS targets are still met exactly**, and the tilt's move stays
  small: 0.0072 nats for the 136 IRS targets (step 31: 0.0063), 0.0220
  with the wealth targets on the SCF's units (step 31: 0.0186).
- **Net worth starts $9 trillion high** before the tilt ($105.8 trillion
  against the SCF's $96.1 trillion; step 31: $96.7 trillion), in all three
  seeds. The cause found: only 1.2% of SCF units report a business loss,
  against 6.3% on the PUF side, and those few SCF units are rich, so the
  file's 11.6 million loss units inherit their wealth (section 6). This
  needs a design decision (question 1).
- **Net worth of top earners improves**: wages of $200,000 and over hold
  $3.95 million before the tilt against step 31's $3.64 million (SCF:
  $4.36 million).
- **A bigger network** (6 layers, width 256; one seed) does not fit the PUF
  better overall and does not change the conclusions.

## 1. The items added

All from the 2015 PUF, in whole dollars, no missing values in any field
(checked). AGI is now wages + taxable interest + ordinary dividends +
the four capital gains pieces + the nine new AGI parts (adjustments with a
minus sign) + other income, and this holds to the dollar on all 207,692 PUF
records (checked). "Other income" is now only what is left: line 21 other
income, the small adjustments the PUF has no field for, and similar; it is
nonzero on 49% of PUF-side units, mostly small.

| Item | PUF fields | In AGI | Why |
|---|---|---|---|
| business_income | E00900 + E02000 + E02100 (Schedules C, E and F, net) | yes | the largest piece of the old other income; the SCF measures it (see section 2 for why one item, not three) |
| retirement_gross | E02400 + E01500 (gross Social Security + gross pensions and annuities) | no | the SCF measures it (X5722); the main income of retirees, who hold much of the wealth |
| tax_exempt_interest | E00400 | no | the SCF measures it (X5706); held by few, very wealthy units |
| other_gains | E01200 (Form 4797) | yes | a 1040 line; PUF-only |
| ira_taxable | E01400 | yes | a 1040 line; PUF-only |
| pensions_taxable | E01700 | yes | a 1040 line; PUF-only |
| ss_taxable | E02500 | yes | a 1040 line; PUF-only (a tax-law calculation from gross benefits; see questions) |
| unemployment | E02300 | yes | a 1040 line; PUF-only |
| alimony | E00800 | yes | a 1040 line; PUF-only |
| state_refunds | E00700 | yes | a 1040 line; PUF-only |
| adjustments | E03150 + E03210 + E03220 + E03230 + E03240 + E03260 + E03270 + E03290 + E03300 + E03400 + E03500 | subtracted | the adjustments to income the PUF itemizes; PUF-only |

Not added: age and sex (largely missing on the PUF), itemized deductions
(saved for later), qualified dividends (a part of ordinary dividends: it
would be a part drawn beside its total, against plan decision 12).

Against SOI 2015 (Table 1.4) the PUF filers match the published counts and
amounts closely for every item except other_gains (Form 4797): the PUF's
E01200 adds to $1.3 billion against the IRS's $11.9 billion net. Full
table: `$R/step33/tables_checks.md`.

**Crude non-filers** (step 23's CPS rows): business_income = sole
proprietorship + rent + rental loss; pensions_taxable = taxable pension
distributions; unemployment = unemployment insurance; retirement_gross =
gross Social Security + gross pension distributions (all in 2015 dollars by
step 23's CPI factor); every other new item zero, because the CPS pool has
no field for it (step 23 already took taxable Social Security as zero).
Their other income becomes the CPS "other income" field, so their AGI is
unchanged (checked to the cent). No missing values anywhere.

## 2. The SCF side (settings sheet `33_settings_scf.json`)

Three items are shared with the SCF; the other eight are PUF-owned (absent
on SCF records, as other income is). Each SCF item is a household amount
for the tax year before the interview, split into tax units by the
production builder's own rules (`33_build_scf.R`), exactly as wages,
interest, dividends and capital gains were; the unit amounts add back to
the household amount in every wave (checked). Wages and capital gains came
out identical to table E27 (checked), so the earlier variables are
untouched. All three are taken as reported (no cell-method fill-in).

| Item | SCF items | PUF fields | Dollar rule | Taken as reported or filled in |
|---|---|---|---|---|
| business_income | X5704 (self-employment, professional practice, business, limited partnership, farm) + X5714 (net rent, trusts, royalties); X5704 split by each spouse's business income, X5714 by earned-income share | E00900 + E02000 + E02100 | **rank** (see below) | as reported |
| retirement_gross | X5722 (pensions, annuities; the SCF's summary variable SSRETINC built from it is described as Social Security and other pensions, annuities, disability and retirement programs) | E02400 + E01500 | unchanged (the PUF's cut points grown by the IRS growth in the item) | as reported |
| tax_exempt_interest | X5706 (non-taxable investment income) | E00400 | unchanged | as reported |

Fixed conversion: a reported value of exactly −1 is read as "nothing" and
set to 0, as the earlier steps did for X5712 (X5704: 131 such rows in the
2016 wave; X5714: 230; X5722: 5 in 2022; not confirmed in the codebook).
Losses are kept.

**Why business income is one item.** The SCF's two questions do not split
along the PUF's schedule lines. In the 2016 wave, X5714 (net rent, trusts,
royalties) adds to about $1.15 trillion, against about $80 billion for
Schedule E parts I and III (rents, royalties, estates, trusts) on 2015
returns; X5704 adds to about $0.58 trillion against about $0.95 trillion for
Schedules C, E part II (partnerships, S corporations) and F. Respondents
evidently report much partnership and S corporation income under rent,
trusts and royalties. Together the SCF has $1.73 trillion against the PUF
side's $1.15 trillion (filers $1.02 trillion; the IRS's net figure for
2015 is $1.03 trillion).

**The dollar rule for business income: rank.** In each wave, SCF units with
positive business income are put in slices by their weighted rank among
positives, so that a slice holds the same share of positive holders in the
SCF as in the PUF; losses likewise among losses. The share with income, with
a loss, and with none is taken as reported. Reason: the SCF's amounts are
far above the PUF's at every point (2016 wave against 2015 PUF side,
positive amounts: median $11,500 against $4,530; 90th percentile $102,500
against $52,370; 99th percentile $1.0 million against $402,300), for
reasons that are not a difference in who has the income (income not
reported to the IRS, different treatment of depreciation and expenses,
rounding). Placing SCF dollars on the PUF's dollar cut points unchanged
would put most SCF business owners in the PUF's top slices. The setting
is user-supplied in the sheet: "rank", "unchanged", or a number (a factor
applied to SCF dollars before the PUF's cut points).

Holders as reported (share of units with a nonzero amount):

| | PUF side, 2015 | SCF 2013 wave | 2016 | 2019 | 2022 |
|---|---:|---:|---:|---:|---:|
| business_income | 24.3% | 17.0% | 20.5% | 20.7% | 21.8% |
| retirement_gross | 29.0% | 33.4% | 33.9% | 33.3% | 34.2% |
| tax_exempt_interest | 3.2% | 2.3% | 2.2% | 2.4% | 2.4% |

Business losses are the large difference: 1.2% of SCF units (2019 wave)
report a business loss, against 6.3% of PUF-side units (section 6).

**Growth for the SCF cut points** (retirement and tax-exempt interest):
the IRS total for the wave's tax year over 2015, as E27 does for interest
and dividends. The IRS file has no total-pensions row for 2018, so the 2018
factor for retirement_gross uses Social Security alone (1.12).

**2019 dollars** (`map/V33/growth_factors_33.csv`): each new item's 2015
map is grown by the IRS growth in the amount per return with the item,
2015 to 2019, as 04_dollars.py does for the old items: business income
1.033, retirement_gross 1.095, tax-exempt interest 0.929, other gains
1.80, taxable IRA 1.162, taxable pensions 1.134, taxable Social Security
1.138, unemployment 1.118, alimony 1.29, state refunds 0.84, adjustments
0.952, and the remainder with the IRS "other income" line, 0.958. The old
items' maps are step 27's, unchanged (checked row for row).

## 3. What ran

1. Table E33 (`33_tables.py`), the 2019 maps `map/V33`.
2. Pretraining: step 31's code unchanged (`31_train.py`, the cell method
   for interest, dividends and capital gains with step 31's settings), on
   table E33, 80 passes, source label on, every record but the 2% held-back
   slice. Base network as step 31 (4 layers, width 128): three seeds, Ns1,
   Ns2, Ns3 (step 31's seeds). Bigger network (6 layers, width 256, 8
   attention heads, feed-forward width 512): Bs1, and Bs2 as a second seed
   (see section 7 for which finished).
3. Generation: 400,000 PUF-style records per run, step 31's settings
   (net worth drawn under the SCF label with the 2016 wave's year, the
   PUF-owned slots left out of that draw), except the third of the four
   oversampled parts is stratified on business income instead of other
   income (other income is now only a small remainder).
4. 2019 dollars from `map/V33`; the nine new AGI parts are added back into
   other income before AGI is computed, so the 136 IRS targets are computed
   exactly as before.
5. The tilt to the 136 IRS targets for 2019 plus the eight SCF wealth
   targets, with the wealth counts on the SCF's units and, separately, on
   the file's units. One extra tilt (Ns1): the 136 targets plus SOI
   tax-exempt interest counts and amounts by AGI bracket (01_targets.py
   already had them; no other new item has targets there).
6. The report (`33_report.py`): PUF fit, relationships, net worth.

A 4-pass test of the whole chain ran first and passed.

## 4. The PUF fit (base network, three seeds)

Generated file (2015 dollars, the stack's own weights, before any
reweighting) against the real PUF side of table E33 (filers and crude
non-filers, survey weights). Means are over all units; percentiles are of
the nonzero amounts. Generated: mean of the three seeds.

| Item | Share nonzero, PUF | Generated | Mean, PUF | Generated | P50, PUF | Generated | P90, PUF | Generated | P99, PUF | Generated |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| wages | 74.1% | 74.0% | $40,751 | $40,262 | $33,200 | $32,831 | $116,200 | $115,700 | $334,300 | $327,500 |
| interest | 25.9% | 25.0% | $517 | $521 | $80 | $80 | $2,130 | $2,183 | $28,100 | $29,033 |
| dividends | 15.9% | 15.2% | $1,352 | $1,257 | $820 | $830 | $14,400 | $14,632 | $102,700 | $102,067 |
| capital_gains_line13 | 13.4% | 12.7% | $3,442 | $2,982 | $540 | $845 | $23,200 | $25,530 | $313,000 | $346,133 |
| kg_st | 6.0% | 5.6% | −$235 | −$193 | −$50 | −$60 | $2,200 | $2,010 | $41,000 | $40,900 |
| kg_lt | 9.0% | 8.6% | $3,500 | $2,985 | $2,470 | $2,540 | $43,700 | $42,800 | $505,300 | $478,700 |
| kg_dist | 2.4% | 2.3% | $65 | $60 | $750 | $773 | $6,050 | $6,173 | $27,800 | $26,667 |
| kg_other | 5.4% | 5.1% | $111 | $129 | −$40 | −$37 | $6,830 | $6,070 | $113,000 | $106,500 |
| business_income | 24.3% | 24.0% | $6,330 | $6,173 | $4,530 | $4,700 | $52,370 | $52,000 | $402,300 | $390,133 |
| retirement_gross | 29.0% | 28.1% | $11,129 | $10,517 | $23,207 | $22,993 | $73,600 | $73,000 | $268,400 | $248,767 |
| tax_exempt_interest | 3.2% | 3.1% | $331 | $371 | $1,380 | $1,507 | $18,000 | $19,367 | $149,300 | $171,733 |
| other_gains | 1.2% | 1.1% | $7 | $11 | −$2 | −$2 | $15,800 | $15,867 | $150,400 | $158,200 |
| ira_taxable | 7.8% | 7.5% | $1,381 | $1,315 | $8,110 | $8,083 | $42,500 | $42,100 | $137,600 | $134,633 |
| pensions_taxable | 17.1% | 16.9% | $4,169 | $4,191 | $15,700 | $16,000 | $57,500 | $58,967 | $129,900 | $134,933 |
| ss_taxable | 10.8% | 10.4% | $1,522 | $1,445 | $12,400 | $12,033 | $29,600 | $29,467 | $42,800 | $42,900 |
| unemployment | 3.7% | 3.6% | $163 | $157 | $3,320 | $3,270 | $10,095 | $9,955 | $17,300 | $17,064 |
| alimony | 0.2% | 0.2% | $48 | $53 | $12,000 | $11,333 | $49,600 | $51,367 | $180,000 | $213,400 |
| state_refunds | 11.3% | 11.1% | $168 | $175 | $660 | $667 | $2,520 | $2,593 | $11,900 | $13,100 |
| adjustments | 20.7% | 20.7% | $759 | $762 | $950 | $927 | $7,050 | $7,053 | $49,200 | $52,197 |
| remainder | 49.2% | 48.9% | −$851 | −$982 | $10 | $3 | $2,560 | $2,400 | $19,930 | $19,113 |
| agi | 92.1% | 91.9% | $58,239 | $56,797 | $33,843 | $33,425 | $127,800 | $127,289 | $436,400 | $434,809 |

Ratios of generated to real for every item, with step 31 beside it for the
items both have (mean of seeds):

| Item | Share nonzero, real | Step 31 | Step 33 | Mean ratio, step 31 | Step 33 | P50 ratio, step 31 | Step 33 | P90 ratio, step 31 | Step 33 | P99 ratio, step 31 | Step 33 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| wages | 74.1% | 73.6% | 74.0% | 0.98 | 0.99 | 0.98 | 0.99 | 0.98 | 1.00 | 0.97 | 0.98 |
| interest | 25.9% | 25.0% | 25.0% | 0.99 | 1.01 | 1.00 | 1.00 | 1.06 | 1.03 | 1.02 | 1.03 |
| dividends | 15.9% | 15.2% | 15.2% | 1.04 | 0.93 | 1.00 | 1.01 | 1.02 | 1.02 | 1.08 | 0.99 |
| capital_gains_line13 | 13.4% | 12.7% | 12.7% | 0.91 | 0.87 | 1.44 | 1.57 | 1.11 | 1.10 | 1.08 | 1.11 |
| kg_st | 6.0% | 5.6% | 5.6% | 0.92 | 0.82 | 1.00 | 1.20 | 0.88 | 0.91 | 0.98 | 1.00 |
| kg_lt | 9.0% | 8.6% | 8.6% | 0.90 | 0.85 | 1.00 | 1.03 | 0.99 | 0.98 | 0.96 | 0.95 |
| kg_dist | 2.4% | 2.3% | 2.3% | 0.88 | 0.93 | 0.94 | 1.03 | 0.94 | 1.02 | 0.89 | 0.96 |
| kg_other | 5.4% | 5.1% | 5.1% | 1.07 | 1.16 | 1.08 | 0.92 | 0.93 | 0.89 | 1.02 | 0.94 |
| business_income | 24.3% |  | 24.0% |  | 0.98 |  | 1.04 |  | 0.99 |  | 0.97 |
| retirement_gross | 29.0% |  | 28.1% |  | 0.94 |  | 0.99 |  | 0.99 |  | 0.93 |
| tax_exempt_interest | 3.2% |  | 3.1% |  | 1.12 |  | 1.09 |  | 1.08 |  | 1.15 |
| other_gains | 1.2% |  | 1.1% |  | 1.54 |  | 1.00 |  | 1.00 |  | 1.05 |
| ira_taxable | 7.8% |  | 7.5% |  | 0.95 |  | 1.00 |  | 0.99 |  | 0.98 |
| pensions_taxable | 17.1% |  | 16.9% |  | 1.01 |  | 1.02 |  | 1.03 |  | 1.04 |
| ss_taxable | 10.8% |  | 10.4% |  | 0.95 |  | 0.97 |  | 1.00 |  | 1.00 |
| unemployment | 3.7% |  | 3.6% |  | 0.96 |  | 0.98 |  | 0.99 |  | 0.99 |
| alimony | 0.2% |  | 0.2% |  | 1.11 |  | 0.94 |  | 1.04 |  | 1.19 |
| state_refunds | 11.3% |  | 11.1% |  | 1.04 |  | 1.01 |  | 1.03 |  | 1.10 |
| adjustments | 20.7% |  | 20.7% |  | 1.00 |  | 0.98 |  | 1.00 |  | 1.06 |
| remainder | 49.2% |  | 48.9% |  | 1.15 |  | 0.27 |  | 0.94 |  | 0.96 |
| agi | 92.1% | 91.7% | 91.9% | 0.98 | 0.98 | 0.98 | 0.99 | 0.98 | 1.00 | 0.99 | 1.00 |

What this shows:

- The new items are reproduced closely: share nonzero within about one
  point and the 50th and 90th percentiles within about 5% for every item
  but the rare ones. Business income, the largest, is within 4% on the mean
  and every percentile shown. Retirement income is 6% low on the mean and
  7% low at the 99th percentile.
- Tax-exempt interest comes out about 12% high on the mean (percentiles 8%
  to 15% high). Other gains (Form 4797) and alimony are rare and the mean
  of other gains nets gains against losses, so their mean ratios swing.
- The items step 31 already had fit about as well as in step 31. Wages,
  interest and AGI are unchanged. The median of nonzero line-13 capital
  gains is high in both steps ($845 against $540; step 31 about $780). Dividends and the capital gains pieces
  lose a little on the mean (dividends 0.93 against 1.04; line-13 capital
  gains 0.87 against 0.91); three seeds per step, so part of this is noise.
- The remainder ("other income" after the eleven items) is nonzero on 49%
  of units but its median nonzero size is about $10: most of it is most
  likely rounding in the PUF's separately rounded fields, plus line-21
  items and net operating losses (negative on 24% of units, mean −$851).

Relationships (base network, every seed):

- Business income by wage group: share with it and mean within a point or
  two and a few percent in every group below $200,000; for wages of
  $200,000 and over the mean is $37,000 to $51,000 against $58,000 on the
  PUF (share with it 43.5% to 46.3% against 44.2%).
- Retirement income by wage group: close everywhere except wages of
  $200,000 and over: mean $15,700 to $17,600 against $25,700.
- Items held together (share of holders of the first who also have the
  second, PUF against the three seeds): business income and adjustments
  52% against 52–54%; retirement and taxable Social Security 37% against
  37%; retirement and taxable pensions 59% against 59–61%; tax-exempt
  interest and dividends 96% against 93–94%; business income and capital
  gains 25% against 23–24%.
- Taxable Social Security with no gross Social Security or pensions (cannot
  happen): 0.01% of units. Taxable Social Security above 85% of gross
  Social Security plus pensions (a loose bound): 0.9% to 1.0% against 0.7%
  on the PUF.
- AGI: below zero on 1.1% to 1.2% of units against 0.9%; 50th, 90th and
  99th percentiles within 2%.

## 5. Net worth and the tilt, against step 31

Nominal 2019 dollars. Mean of three seeds (range in brackets where it
matters). Step 31 = the cell method's Ks1-3, recomputed by the same report
code.

| | SCF 2019 | Step 31 | Step 33 |
|---|---:|---:|---:|
| Total net worth before the tilt | $96.1T | $96.7T (92.2–102.4) | $105.8T (104.5–107.4) |
| Mean net worth, units with none of interest, dividends, gains, before | | $102,500 | $120,100 (109,300–126,800) |
| Mean net worth, wages $200k and over, before | $4.36 million | $3.64 million | $3.95 million (3.78–4.08) |
| Move, 136 IRS targets alone | | 0.0063 nats | 0.0072 nats |
| Move, 136 IRS + 8 wealth targets, counts on SCF units | | 0.0186 nats | 0.0220 nats |
| Records effectively kept, same | | 95.6% | 95.2% |
| Move, counts on the file's units | | 0.0447 nats | 0.0502 nats |
| Records effectively kept, same | | 91.4% | 90.3% |
| Median after the tilt, counts on SCF units / file units | $91,100 | $38,100 / $90,900 | $37,900 / $90,800 |
| Largest miss on the 136 IRS targets after the tilt | | 0.0000% | 0.0000% |
| Move with tax-exempt interest targets added (Ns1 only, SCF units; Ns1 without them: 0.0065 / 0.0220) | | | 0.0068 / 0.0223 nats |

By wage group before the tilt (mean net worth):

| Wages | SCF 2019 | Step 31 | Step 33 |
|---|---:|---:|---:|
| none | $761,000 | $785,000 | $835,000 |
| $1–25k | $222,000 | $201,000 | $203,000 |
| $25–50k | $154,000 | $188,000 | $210,000 |
| $50–100k | $332,000 | $324,000 | $385,000 |
| $100–200k | $840,000 | $784,000 | $889,000 |
| $200k and over | $4.36 million | $3.64 million | $3.95 million |

After the tilt both steps hit the SCF's totals, and the IRS targets are met
exactly in every run. The tilt has to move step 33's file about 18% more
than step 31's (0.0220 against 0.0186 nats) because it starts $9 trillion
high.

## 6. Why net worth starts $9 trillion high: business losses

Net worth by business income group (2019 dollars, mean per unit; groups by
rank within each file, since business income is matched by rank):

| Group | SCF 2019 wave: share of units | Mean net worth | Step 33 file before the tilt (three seeds): share | Mean net worth |
|---|---:|---:|---:|---:|
| No business income | 79.3% | $302,000 | 75.2–76.4% | $263,000–273,000 |
| Business loss | 1.2% | $2.71 million | 6.2–6.4% | $1.53–1.78 million |
| Positive, bottom half | 10.0% | $708,000 | 8.7–9.2% | $680,000–692,000 |
| Positive, 50th–90th percentile | 7.5% | $1.68 million | 7.0–7.3% | $1.04–1.07 million |
| Positive, top 10% | 1.9% | $10.3 million | 1.7–1.8% | $6.6–7.4 million |

The SCF's units differ from the file's (143 million against 187 million;
the file adds the non-filers and the PUF's other units), so the shares are
not expected to match; the loss row is the point. Only 1.2% of SCF units
report a business loss, and they are rich ($2.7 million on average). On the
PUF 6.3% of units have one, most of them small Schedule C or rental losses.
Because the model learns net worth given a loss only from the SCF's few
loss reporters, the file's 11.6 million loss units get about $1.6 million
each, about $19 trillion in all, against about $4.6 trillion held by the
SCF's loss reporters. That difference is larger than the whole $9 trillion
excess; the other groups come out a little below the SCF's. The likely reason is that SCF
households with small business or rental losses report zero or a small
positive amount: the SCF under-reports losses the way it under-reports
small interest and dividends. Possible fixes are design questions for
John (section 8).

By retirement income: the file gives units with large retirement income
more net worth than the SCF does (top 10% of holders: $2.4–2.7 million
against $1.9 million), and units with none less ($400,000 against
$605,000). By tax-exempt interest the pattern is close to the SCF's except
the top two groups, which come out lower ($5–6 million against $9.9
million; $16–19 million against $26.5 million).

## 7. A bigger network

Bs1: 6 layers instead of 4, width 256 instead of 128, 8 attention heads,
feed-forward width 512, seed 20261016, otherwise exactly as Ns1. 123
minutes of training against 67 for Ns1 (GPU types not compared). One seed only; Bs2 (seed 20261017) was still
training when this was written (`33_STATUS.md`) and the chain's last report
job will add it to `$R/step33/report.md`.

Held-back loss after 80 passes (lower is better; the 2% of records never
trained on): Bs1 0.8779, base network 0.8789, 0.8746 and 0.8835 (seeds 1
to 3). The bigger network is inside the range of the base network's seeds.

PUF fit, Bs1 (ratios generated to real; the step 31 columns as in section
4; "Step 33" here is Bs1):

| Item | Share nonzero, real | Step 31 | Bs1 | Mean ratio, step 31 | Bs1 | P50 ratio, step 31 | Bs1 | P90 ratio, step 31 | Bs1 | P99 ratio, step 31 | Bs1 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| wages | 74.1% | 73.6% | 73.7% | 0.98 | 0.97 | 0.98 | 0.98 | 0.98 | 0.99 | 0.97 | 0.98 |
| interest | 25.9% | 25.0% | 25.2% | 0.99 | 1.06 | 1.00 | 1.00 | 1.06 | 1.02 | 1.02 | 1.06 |
| dividends | 15.9% | 15.2% | 15.5% | 1.04 | 0.94 | 1.00 | 1.09 | 1.02 | 1.06 | 1.08 | 1.01 |
| capital_gains_line13 | 13.4% | 12.7% | 13.1% | 0.91 | 0.79 | 1.44 | 1.44 | 1.11 | 1.01 | 1.08 | 0.98 |
| kg_st | 6.0% | 5.6% | 5.8% | 0.92 | 1.07 | 1.00 | 1.20 | 0.88 | 0.92 | 0.98 | 1.01 |
| kg_lt | 9.0% | 8.6% | 8.7% | 0.90 | 0.80 | 1.00 | 0.98 | 0.99 | 0.96 | 0.96 | 0.92 |
| kg_dist | 2.4% | 2.3% | 2.4% | 0.88 | 0.97 | 0.94 | 1.04 | 0.94 | 1.00 | 0.89 | 0.94 |
| kg_other | 5.4% | 5.1% | 5.2% | 1.07 | 0.86 | 1.08 | 0.75 | 0.93 | 1.02 | 1.02 | 1.14 |
| business_income | 24.3% |  | 24.0% |  | 0.94 |  | 0.98 |  | 0.98 |  | 0.96 |
| retirement_gross | 29.0% |  | 28.9% |  | 0.96 |  | 0.98 |  | 0.99 |  | 0.93 |
| tax_exempt_interest | 3.2% |  | 3.3% |  | 1.21 |  | 1.03 |  | 1.08 |  | 1.12 |
| other_gains | 1.2% |  | 1.2% |  | 9.90 |  | 1.00 |  | 1.10 |  | 1.22 |
| ira_taxable | 7.8% |  | 7.6% |  | 0.96 |  | 0.99 |  | 1.00 |  | 1.02 |
| pensions_taxable | 17.1% |  | 16.9% |  | 1.01 |  | 1.03 |  | 1.01 |  | 1.04 |
| ss_taxable | 10.8% |  | 10.5% |  | 0.98 |  | 1.01 |  | 1.01 |  | 0.99 |
| unemployment | 3.7% |  | 3.5% |  | 0.94 |  | 0.99 |  | 1.00 |  | 0.99 |
| alimony | 0.2% |  | 0.2% |  | 1.20 |  | 1.00 |  | 1.11 |  | 1.67 |
| state_refunds | 11.3% |  | 10.9% |  | 1.06 |  | 1.02 |  | 1.03 |  | 1.11 |
| adjustments | 20.7% |  | 20.5% |  | 1.03 |  | 1.00 |  | 1.00 |  | 1.07 |
| remainder | 49.2% |  | 48.7% |  | 1.19 |  | 1.00 |  | 0.98 |  | 0.97 |
| agi | 92.1% | 91.7% | 91.8% | 0.98 | 0.96 | 0.98 | 0.98 | 0.98 | 0.99 | 0.99 | 0.98 |

Against the base network's three seeds (section 4), the bigger network is
better on some items and worse on others, with no pattern: closer on the
share holding capital gains (13.1% against 12.7%; real 13.4%), retirement
income (28.9% against 28.1%; real 29.0%) and the capital gains 90th and
99th percentiles; further off on the mean of capital gains (0.79 against
0.87), tax-exempt interest (1.21 against 1.12) and the 99th percentile of
alimony (1.67, a rare item). Other gains' mean ratio of 9.9 is a net figure
near zero ($7 a unit on the PUF) and means little. At wages of $200,000
and over, business income is $44,000 against $58,000 (base: $37,000 to
$51,000) and retirement income $22,000 against $25,700 (base: $15,700 to
$17,600), the second a clear gain.

Net worth and the tilt, Bs1 (2019 dollars):

| | SCF 2019 | Step 31 (3 seeds) | Step 33 base (3 seeds) | Step 33 bigger (Bs1) |
|---|---:|---:|---:|---:|
| Total net worth before the tilt | $96.1T | $96.7T | $105.8T | $111.4T |
| Mean net worth, wages $200k and over, before | $4.36 million | $3.64 million | $3.95 million | $4.33 million |
| Mean net worth, none of interest, dividends, gains, before | | $102,500 | $120,100 | $124,000 |
| Move, 136 IRS targets alone | | 0.0063 | 0.0072 | 0.0067 |
| Move, plus wealth targets, counts on SCF units | | 0.0186 | 0.0220 | 0.0272 |
| Move, plus wealth targets, counts on file units | | 0.0447 | 0.0502 | 0.0448 |
| Records effectively kept (SCF units / file units) | | 95.6% / 91.4% | 95.2% / 90.3% | 94.5% / 91.5% |

The business-loss problem of section 6 is the same (6.4% of units with a
loss, $1.75 million mean net worth before the tilt). Net worth of the top
wage group comes out close to the SCF's for the first time, but on one seed
and with the total $15 trillion high. On this evidence a bigger network
does not change the picture: its PUF fit is about the same, and the largest
problem found in this step (business losses) comes from the SCF data, not
from the network's size.

## 8. Questions for John

1. **Business losses in the SCF.** Only 1.2% of SCF units report a
   business loss against 6.3% on the PUF side, and that alone puts net
   worth $9 trillion high before the tilt (section 6). Options: (a) read
   SCF business zeros as possibly hiding a loss and fill them in with the
   cell method, the PUF setting how many hidden loss holders there are in
   each cell (the method would need losses as a range, as capital gains
   already has); (b) treat losses and zeros as one group on the SCF side
   (take only positives as reported); (c) leave it to the tilt. Which?
2. **One business item or three.** The SCF's two questions do not follow
   the PUF's schedules (X5714 "net rent, trusts, royalties" is $1.15
   trillion against $80 billion of Schedule E rents and royalties), so
   step 33 shares one item, Schedules C, E and F together. If the PUF
   side should keep Schedule C, partnerships and S corporations, and rents
   apart, the way to do it is the capital gains pattern: PUF-owned pieces
   with the shared total computed from them on PUF records (plan decision
   12 as narrowed on 18 October). That needs the computed-total code in
   03_generate.py generalised beyond capital gains. Wanted?
3. **The dollar rule for business income.** Rank (chosen) or unchanged or
   a factor? Rank keeps the SCF's share of holders and the PUF's dollar
   shape; it says nothing about whether the SCF's higher amounts are
   real.
4. **Retirement income.** The PUF side is gross Social Security plus gross
   pensions and annuities (E01500), which includes rollovers; the SCF's
   X5722 presumably does not. With taxable pensions (E01700) instead of
   E01500 the concepts would be closer, but then the shared item would
   contain a PUF-owned part (pensions_taxable), a total beside its part.
   Also: is X5722 really Social Security plus pensions? Its label in the
   data file reads "pensions, annuities"; the SCF's own summary variable
   built from it (SSRETINC) is described as including Social Security. I
   have not checked the codebook text.
5. **Taxable Social Security** is a tax-law calculation from gross
   benefits and other income. Should it be computed in Tax-Simulator
   rather than drawn? Drawn, it breaks the 85% ceiling on about 1% of
   units (0.7% on the real PUF by the same loose test, which mixes in
   pensions).
6. **SCF reported value −1** for X5704 and X5714 (131 and 230 rows in the
   2016 wave) was read as "nothing", as earlier steps did for X5712. Not
   checked against the codebook.
7. **Non-filers.** The crude CPS non-filers get the new items the CPS has
   (business, taxable pensions, unemployment, gross Social Security and
   pensions) and zero for the rest. The non-filer work proper should carry
   these.
8. **Tax-exempt interest** has SOI targets by AGI bracket; adding them
   (Ns1, wealth counts on SCF units) moves the file 0.0068 nats for the IRS
   targets and 0.0223 with the wealth targets, against 0.0065 and 0.0220
   for the same run without them. Keep them in the standard target list?

## 9. Files

Code: `33_build_scf.R`, `33_scf.sbatch`, `33_tables.py`,
`33_settings_scf.json`, `33_env.sh`, `33_run.sbatch`, `33_tilt.sbatch`,
`33_cpu.sbatch`, `33_submit.sh`, `33_report.py`; training is `31_train.py`
unchanged. Changes to shared code are listed in `33_STATUS.md`.
Scratch: table `data/stacked_E33.parquet`, `data/band_edges_E33.json`;
maps `map/V33/`; models `model/step33/<run>/`; files
`gen/gen_puf_stack_s33_<run>.parquet`; tilts `tilt/step33/`; report
`step33/report.md` (the three base seeds alone: `step33/base/report.md`);
table checks `step33/tables_checks.md`.
