#------------------------------------------------------------------------------
# tail_common.R -- shared by the top-tail diagnostics 07, 08 and 09
#
# One loader for a vintage's filers and one weighted top-N slice, so the three
# scripts cannot drift on the universe or on the marginal-record rule.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(dplyr) })
source('src/nonfilers/sw_common.R')   # model_data_path(), raw_data_root()
source('src/agi_proxy.R')             # agi_proxy(), AGI_PROXY_INPUTS

# The columns the diagnostics read: identity, universe flags, and everything
# agi_proxy() needs (which covers every Table 4.3 line and 09's scaled columns)
TAIL_COLS = c('id', 'weight', 'filer', 'dep_status', 'forbes_flag', AGI_PROXY_INPUTS)

vintage_dir = function(vintage) model_data_path('Tax-Data', 'v1', vintage, 'baseline')

# One year's filers with positive weight from a published vintage, raw (no
# proxy). `dependents = FALSE` drops dependent returns, Table 4.3's universe.
read_filers = function(vintage, year, dependents = TRUE) {
  d = fread(file.path(vintage_dir(vintage), sprintf('tax_units_%d.csv', year)), select = TAIL_COLS)
  stopifnot(!anyNA(d$forbes_flag), !anyNA(d$dep_status), all(d[forbes_flag == 1, filer] == 1))
  d = d[filer == 1 & weight > 0]
  if (!dependents) d = d[dep_status == 0]
  d
}

# The weighted top n_top returns by `agi`, the marginal record taken in part:
# rows in the slice with `w_in`, the weight each contributes.
top_slice = function(d, n_top) {
  o = d[order(-agi)][, cum_w := cumsum(weight)]
  o[, w_in := pmin(weight, pmax(n_top - (cum_w - weight), 0))]
  o[w_in > 0]
}
