# The package for a holder of the 2015 PUF

What CRS (or anyone with their own license to the 2015 IRS Public Use File) receives from us, and how they use it.
John's plan of 4 November 2026. The pipeline makes it when settings `generation.filers` is `real_puf`
(FEATURES.md, "The real PUF as the filers").

## Why a package

With `filers: real_puf` every 2015 PUF record is in the year's file: each record keeps its own PUF items, grown to
the year, and our three networks draw everything the PUF lacks given the record. A holder of the PUF can then rebuild
our year's Tax-Simulator file exactly from their own PUF and a set of files that contains no PUF value. They need no
network, no GPU and none of our other data.

## What they receive, one folder per year

| File | What it holds |
|---|---|
| `README.md` | the year's own description: how the file was made, each file, how to run the merge, the round-trip result |
| `factors.parquet`, `factors.csv` | one row per PUF item and step, with a factor for positive amounts and one for negative amounts, the same for every record. Always relative to the 2015 PUF |
| `records.parquet` | one row per PUF record copy in the file: RECID and copy, the weight factor, the refund multiplier, the items we added, and a few Tax-Simulator columns as values |
| `extra_records.parquet` | the records that are not PUF records: non-filers and the Forbes list of billionaires, every Tax-Simulator column |
| `spec.json` | the definitions: how each 2015 item is made from PUF fields, how the computed columns are made, where each Tax-Simulator column comes from |
| `merge_puf.py`, `merge_puf.R` | the merge, in Python and in R |

Each PUF record appears three times, once per network (column `copy`), at a third of its weight. The factors chain
the years the file was built through: in a past year (2015-2023), the growth from 2015 to the year; in 2024, the
growth from 2015 to 2023 and then from 2023 to 2024; in 2025-2075, those and then the growth from 2024 to the year (the
jump-off rule, TARGETS.md S13). The six Schedule A multipliers of the year the records were drawn are one more step on
six itemized items. A deduction the law has repealed by the year is one more step with a factor of zero from the
repeal year (John, 5 November): the domestic production activities deduction from 2018, the tuition and fees
deduction from 2021 (settings `taxes.repealed_adjustments`).

## How they use it

    python merge_puf.py puf_2015.csv <package folder> tax_units_<year>.csv
    Rscript merge_puf.R puf_2015.csv <package folder> tax_units_<year>.csv

The output is our `tax_units_<year>.csv`, the file Tax-Simulator reads (production's 188 columns, in its order, with
its conventions for missing values). An output name ending in `.parquet` writes Parquet, which keeps every number
exactly; no extension writes both.

## How the values that change beyond a growth factor are shipped

Most PUF items are their 2015 amount times one growth factor per year and sign. These rules change an item by more
than that; each is shipped in the form that says least about any record (John's decisions of 5 November for 1, 3
and 4):

1. **Taxable state refunds.** Two rules cut the refund. The refund rule (decision 47): a refund is taxable only to a
   return that deducted the tax by itemizing, so the pipeline removes it on returns our calculator finds do not
   itemize in the year. The tax benefit rule (version A, from 2019): an itemizer's refund is taxable only as far as
   deducting the tax lowered tax, so under the cap on the state and local tax deduction part or all of it can go;
   15% of the refunds this rule would cut are kept whole, because SOI shows returns reporting them anyway. The
   package ships the outcome as one multiplier per record copy and year, `state_ref_multiplier`, between 0 and 1: 0
   where the refund is removed, 1 where it is kept whole, the share kept where the tax benefit rule keeps part. The
   merge multiplies the grown refund by it and computes other income and taxable Social Security again; it does no
   tax calculation for this item.
2. **The Schedule A multipliers** (decision 49: medical expenses, state and local income or sales tax, real estate
   tax, mortgage interest, cash and other gifts, each scaled so that the average deduction per itemizer is the IRS's):
   six numbers per year, a step in the factor file, the same for every record.
3. **Repealed deductions** (the domestic production activities deduction from 2018, tuition and fees from 2021): a
   step of factor zero in the factor file from the repeal year, the same for every record.
4. **The deductible half of self-employment tax** (E03260): not shipped. The pipeline's file holds our calculator's
   amount (settings `taxes.se_tax_ded_from_calculator`), no column of the Tax-Simulator file holds it, and the
   recipient's tax calculator computes it from the self-employment income and wages, as ours does. Its factor rows
   are the plain growth, so the merge's working items carry the PUF's own amount grown, which no output column reads;
   the factor check counts the record copies where that differs from our file and does not stop on them.
5. **The AMT net operating loss adjustment** (P60100): a rule the merge applies, not values. The adjustment, grown
   like any item, is held to at most the record's net operating loss, the negative part of its other income (the PUF
   has no field of its own for the loss).

Capital gains after the loss limit, other income in AGI and taxable Social Security are recomputed by the merge from
the shipped items, not shipped.

## The test

The package stage fails unless the merge, run on our own copy of the 2015 PUF, gives the year's Tax-Simulator file:
every column equal on every record, or different by rounding only (at most one millionth of a dollar or one part in
10^12). The stage runs both scripts. The differences that remain come from adding the same numbers in another order,
and from weights that no factor reproduces to the last binary digit; each year's README and the files
`roundtrip_<run>_python.csv` and `roundtrip_<run>_r.csv` beside the package list them by column. On 4 November
(2019, 2023, 2024, 2030 and 2075) both scripts gave 187 of the 188 columns identical on every record; the weight was
one last binary digit off on about 12% of record copies (scratch `explore/2026-11-04/realpuf_setting/RESULTS.md`).
The deductible half of self-employment tax is the stated exception: it is not shipped and no Tax-Simulator column
holds it, so the test compares every other column.

Run6 (5 November, with the three changes above; scratch `explore/2026-11-05/run6/RESULTS.md` and
`package_results.csv`): every year 2015-2075 passed with both scripts. 2015-2018: 187 of 188 columns identical on
every record, the weight off by rounding only. 2019 on: 186 identical; the weight and the taxable state refund off by
rounding only (the refund on 7 to 218 record copies a year, by one last binary digit: the grown refund times its
multiplier is not always exactly the file's refund). Each year about 11,700 (2015-2017) to 144,000 (2075) record
copies have the refund removed, and up to 6,400 keep part of it. A package is about 70-73 MB a year.

## Confidentiality

No PUF amount of any PUF record is in the package. Three items we add are a PUF amount times or plus something else,
so the merge computes them from the recipient's PUF and the package holds only the other part: the basis of long-term
gains (the package holds the basis-to-sales ratio of the drawn holding period), the farm's wage bill for the qualified
business income deduction (it is the farm income), and the S corporation's wage bill (the package holds the outside
workers' pay and a 0/1 flag for adding the owner's wages). Parts of the package still depend on PUF values: the refund
multiplier (whether the record itemizes, and the share of its refund the tax benefit rule keeps), whether a record has a long-term gain (its holding period is missing where it has none), and items computed from
the record's income rank (death probabilities, parents' income rank). The non-filers' tax items are amounts picked
from PUF records with similar items, grown, as in every file the pipeline makes. The package folders are readable by the owner only
(chmod 700), and the recipient should treat the package as PUF-derived data under their license.
