#--------------------------------------
# wealth_schema.R
#
# Three coordinate systems for the SCF
# wealth imputation:
#
#   wealth_y_vars       — IMPUTATION schema (24 vars).
#                          What the DRF + Stage 3 tilt + Step B
#                          rescale operate on. Uses SCF-Bulletin-
#                          native names (cash, equities, bonds,
#                          dc, db, ..., kg_primary_home, ...).
#                          Sourced by impute_variables.R to
#                          initialize NA placeholders on the 2017
#                          base, and by wealth.R to name DRF
#                          output columns.
#
#   wealth_output_vars  — OUTPUT schema (32 vars).
#                          What run_wealth_imputation() returns
#                          after the post-impute rename. Three
#                          axes: value.* (stocks, 14+6 cols),
#                          basis.* (cost basis, 5 cols), and
#                          accruals.* (annual unrealized-gain
#                          flow, 7 cols). This is the schema
#                          that flows into materialize() and
#                          downstream consumers.
#
# Mapping:
#   asset value cols are paste0('value.', wealth_asset_vars).
#   debt  value cols are paste0('value.', wealth_debt_vars).
#   basis cols are derived in wealth.R per:
#     basis.primary_home  = primary_home  - kg_primary_home
#     basis.pass_throughs = pass_throughs - kg_pass_throughs
#     basis.equities      = equities      - kg_other
#     basis.other_home    = other_home    - kg_other_re * (other_home / (other_home + re_fund))
#     basis.re_fund       = re_fund       - kg_other_re * (re_fund    / (other_home + re_fund))
#   accruals cols are computed in src/imputations/accruals.R per
#     accruals.<x> = rate_<x> × value.<x>     (or per-record blend for dc)
#   wealth_accrual_categories is a strict subset of wealth_asset_vars —
#   the 7 appreciation-bearing categories for which Z.1 long-run mean
#   revaluation rates are nonzero / not conceptually wrong (see
#   docs/wealth_accruals_design.md).
#
# Must match Wealth-Tax-Simulator
# `src/data.R:28–84` (output schema).
#--------------------------------------

wealth_asset_vars = c(
  'cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities',
  'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
  're_fund', 'other_nonfin'
)

wealth_debt_vars = c(
  'primary_mortgage', 'other_mortgage', 'credit_lines',
  'credit_cards', 'installment_debt', 'other_debt'
)

wealth_kg_vars = c(
  'kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other'
)

wealth_y_vars = c(wealth_asset_vars, wealth_debt_vars, wealth_kg_vars)

wealth_accrual_categories = c(
  'equities', 'pass_throughs', 'primary_home', 'other_home', 're_fund',
  'dc', 'trusts'
)

wealth_value_asset_vars = paste0('value.', wealth_asset_vars)
wealth_value_debt_vars  = paste0('value.', wealth_debt_vars)
wealth_basis_vars       = c('basis.primary_home', 'basis.other_home',
                             'basis.re_fund', 'basis.pass_throughs',
                             'basis.equities')
wealth_accrual_vars     = paste0('accruals.', wealth_accrual_categories)

wealth_output_vars = c(wealth_value_asset_vars,
                        wealth_value_debt_vars,
                        wealth_basis_vars,
                        wealth_accrual_vars)
