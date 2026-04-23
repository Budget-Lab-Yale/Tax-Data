#--------------------------------------
# wealth_schema.R
#
# Canonical 23-dim Y-schema for the SCF
# wealth imputation: 13 assets + 6 debts
# + 4 unrealized capital-gains fields.
# Sourced by impute_variables.R (to
# initialize NA placeholder columns on
# the 2017 base) and by wealth.R (to
# name the DRF output columns).
#
# Must match Wealth-Tax-Simulator
# `src/data.R:28–84`.
#--------------------------------------

wealth_asset_vars = c(
  'cash', 'equities', 'bonds', 'retirement', 'life_ins', 'annuities',
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
