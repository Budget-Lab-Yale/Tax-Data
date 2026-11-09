# The package for a holder of the 2015 PUF

What CRS, or anyone with their own license to the 2015 IRS Public Use File, receives, and how they use it. The
pipeline makes the package (`output/package.py`, stage `package`) when settings `generation.filers` is `real_puf`.

With `real_puf`, every 2015 PUF record is in the year's file: each keeps its own PUF items, grown to the year, and the
three networks draw everything the PUF lacks given the record. A holder of the PUF can then rebuild the year's
Tax-Simulator file exactly from their own PUF and a set of files that contains no PUF amount, with no network, no GPU
and none of the other data.

## Contents, one folder per year

| File | What it holds |
|---|---|
| `README.md` | the year's description: how the file was made, each file, how to run the merge, the round-trip result |
| `factors.parquet`, `factors.csv` | one row per PUF item and step, with a factor for positive amounts and one for negative amounts, the same for every record, always relative to the 2015 PUF |
| `records.parquet` | one row per PUF record copy in the file: RECID and copy, the weight factor (the weight over the PUF weight S006 / 100), the refund multiplier, the medical floor used for the 2015 other itemized deductions, the drawn filing status and dependents of the PUF's four aggregate records, the items the networks and other imputations added, and the remaining Tax-Simulator columns as values |
| `extra_records.parquet` | the records that are not PUF records (non-filers and the Forbes list of billionaires), every Tax-Simulator column |
| `spec.json` | the definitions: how each 2015 item is made from PUF fields, how the computed columns are made, where each Tax-Simulator column comes from |
| `merge_puf.py`, `merge_puf.R` | the merge, in Python and in R |

Each PUF record appears three times, once per network (column `copy`), at a third of its weight.

The factors chain the years the file was built through:
- in a past year (2015-2023), the growth from 2015 to the year;
- in 2024, the growth from 2015 to 2023 and then from 2023 to 2024;
- in 2025-2075, those and then the growth from 2024 to the year (the jump-off rule, `TARGETS.md`).

Two more kinds of step are added to the chain, each the same for every record:
- the year's six Schedule A multipliers (medical expenses, state and local income or sales tax, real estate tax,
  mortgage interest, cash and other gifts to charity), which scale each item so that the average deduction per
  itemizer is the IRS's;
- a factor of zero from the year the law repealed a deduction: the domestic production activities deduction from 2018,
  the tuition and fees deduction from 2021 (settings `taxes.repealed_adjustments`).

## Use

    python merge_puf.py puf_2015.csv <package folder> tax_units_<year>.csv
    Rscript merge_puf.R puf_2015.csv <package folder> tax_units_<year>.csv

Each script needs about 16 GB of memory; the Python one needs numpy, pandas and pyarrow, the R one data.table,
jsonlite and arrow. The output is the year's `tax_units_<year>.csv`, the file Tax-Simulator reads (production's 188
columns, in its order, with its conventions for missing values). An output name ending in `.parquet` writes Parquet,
which keeps every number exactly; a name with no extension writes both.

## Values that change by more than a growth factor

Most PUF items are their 2015 amount times one factor per step and sign. The rules below change an item by more than
that; each is shipped in the form that says least about any record.

1. **Taxable state refunds.** A refund is taxable only to a return that deducted the tax by itemizing, so the pipeline
   removes it on returns the calculator finds do not itemize in the year. From 2019, an itemizer's refund is taxable
   only as far as deducting the tax lowered tax, so under the cap on the state and local tax deduction part or all of
   it can go; 15% of the refunds this rule would cut are kept whole (settings `taxes.refund_reported_anyway`), because
   SOI shows returns reporting them anyway. The package ships the outcome as one multiplier per record copy and year,
   `state_ref_multiplier`, between 0 and 1: 0 where the refund is removed, 1 where it is kept whole, the share kept
   where the second rule keeps part. The merge multiplies the grown refund by it and computes other income and taxable
   Social Security again; it does no tax calculation.
2. **The Schedule A multipliers**: six numbers per year, a step in the factor file.
3. **Repealed deductions**: a step of factor zero in the factor file.
4. **The deductible half of self-employment tax** (E03260) is not shipped. The pipeline's file holds the calculator's
   amount (settings `taxes.se_tax_ded_from_calculator`), no Tax-Simulator column holds it, and the recipient's tax
   calculator computes it from the self-employment income and wages. Its factor rows are the plain growth, so the
   merge's working items carry the PUF's own amount grown, which no output column reads.
5. **The AMT net operating loss adjustment** (P60100) is a rule the merge applies, not values: the adjustment, grown
   like any item, is held to at most the record's net operating loss, the negative part of its other income (the PUF
   has no field of its own for the loss).

Capital gains after the loss limit, other income in AGI and taxable Social Security are computed by the merge from
the shipped items, not shipped.

## The round-trip test

The package stage fails unless the merge, run on the pipeline's own copy of the 2015 PUF, gives the year's
Tax-Simulator file: every column equal on every record, or different by rounding only (at most one millionth of a
dollar or one part in 10^12). The stage runs both scripts. The deductible half of self-employment tax is not compared,
since no Tax-Simulator column holds it. Each year's README and the files `roundtrip_<run>_python.csv` and
`roundtrip_<run>_r.csv` beside the package list the remaining differences by column. They come from adding the same
numbers in another order and from weights that no factor reproduces to the last binary digit.

When last run on every year 2015-2075, both scripts passed in every year. In 2015-2018, 187 of the 188 columns were
identical on every record and the weight differed by rounding only. From 2019, 186 were identical; the weight and the
taxable state refund differed by rounding only (the refund on 7 to 218 record copies a year, by one last binary
digit, because the grown refund times its multiplier is not always exactly the file's refund). Each year about 11,700
(2015-2017) to 144,000 (2075) record copies have the refund removed, and up to 6,400 keep part of it. A package is
about 70-73 MB a year.

## Confidentiality

No PUF amount of any PUF record is in the package. Two added items are a PUF amount times something else, so the merge
computes them from the recipient's PUF and the package holds only the other part: the basis of long-term gains (the
package holds the basis-to-sales ratio of the drawn holding period) and the farm's wage bill for the qualified
business income deduction (it is the farm income).

Parts of the package still depend on PUF values:
- the other wage bills (zero where the record has no positive income of the type; a partnership's and an S
  corporation's split by their income);
- the refund multiplier (whether the record itemizes, and the share of its refund the second rule keeps);
- whether a record has a long-term gain (its holding period is missing where it has none);
- items computed from the record's income rank (death probabilities, parents' income rank).

The non-filers' tax items are amounts picked from PUF records with similar items, grown, as in every file the pipeline
makes. The package folders are readable by the owner only (chmod 700), and the recipient should treat the package as
PUF-derived data under their license.
