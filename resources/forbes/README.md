# Forbes Billionaire Splice Resources

`forbes_billionaires_2022_2025.csv` is the input template for synthetic
billionaire tax units. Populate one row per Forbes billionaire per list year.

Required columns:

- `year`: Forbes list year. The v1 splice is wired for 2022-2025.
- `rank`: Forbes rank within year.
- `name`: Forbes display name retained in output metadata.
- `net_worth`: net worth in dollars, not millions or billions.
- `source_category`: Forbes source/category label.

Optional columns:

- `weight`: synthetic record weight. Defaults to 1 when absent.
- `filing_status`, `age1`, `age2`, `male1`, `male2`, `sector`, `country`,
  `notes`: retained for audit/input hygiene, but not consumed by v1.

`bsyz_fiscal_income_params.csv` maps Forbes wealth to fiscal income using
BSYZ 2025 Table 1. The file contains:

- `fiscal_income_to_wealth`: fiscal income divided by Forbes wealth.
- `share`: high-level fiscal-income composition shares.

The paper reports separate values for the top 100 and next 300. The
`rest_billionaires` group currently reuses the next-300 values as a v1
simplification until we choose a better extrapolation below rank 400.
