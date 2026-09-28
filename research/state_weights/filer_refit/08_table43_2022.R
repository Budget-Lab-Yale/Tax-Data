#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 08_table43_2022.R  (follow-up to 07) -- the top percentiles against IRS
# Table 4.3
#
# Pub 1304 Table 4.3 tabulates returns excluding dependents by descending
# cumulative AGI percentile -- top 0.001%, 0.01%, 0.1%, 1%, ... -- with the
# count, the AGI floor, AGI and income items. It is the one published table
# that looks inside the $10M+ class Table 1.4 stops at, so it measures the
# compressed tail directly (research/state_weights/notes/top_tail_backcast.md).
#
#   with     the TY2022 output as emitted (Forbes rows + splice weights)
#   without  Forbes rows dropped, splice weight factors divided back out
#
# Filers excluding dependent returns (Table 4.3's universe); AGI is
# src/agi_proxy.R for 2022; each slice is the weighted top p% of OUR returns,
# so the counts compare our universe to the IRS's and the floor compares tails.
#
# Reads results/table43/22in43ts.xls (https://www.irs.gov/pub/irs-soi/22in43ts.xls,
# downloaded 2026-09-28; compute nodes may lack internet).
# Writes results/table43/table43_2022.csv.
#   sbatch ... research/state_weights/filer_refit/run_table43_2022.sbatch
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(readxl) })
source('src/nonfilers/sw_common.R')   # model_data_root()
source('src/agi_proxy.R')

VINTAGE = '2026092720_filer_refit_v2'
YEAR    = 2022L
RES     = 'research/state_weights/filer_refit/results/table43'
XLS     = file.path(RES, '22in43ts.xls')
PCTILES = c('0.001 percent' = 1e-5, '0.01 percent' = 1e-4, '0.1 percent' = 1e-3, '1 percent' = 1e-2)


#------------------------------------------------------------------------------
# IRS Table 4.3: the top-percentile rows, $ thousands (the floor in dollars)
#------------------------------------------------------------------------------

# Column layout of the TY2022 sheet (verified 2026-09-28): label; returns; AGI
# floor; AGI; then (returns, amount) pairs for wages, taxable interest,
# ordinary dividends, qualified dividends, net capital gain less loss, gains at
# preferential rates, Schedule C net income, Schedule C net loss, partnership
# and S corporation net income, their net loss.
sheet = as.data.frame(read_excel(XLS, col_names = FALSE, .name_repair = 'minimal'))
stopifnot(grepl('Table 4.3', sheet[1, 1]), grepl('Tax Year 2022', sheet[1, 1]))
IRS_COLS = c(returns = 2, agi_floor = 3, agi = 4, wages = 6, txbl_int = 8, div = 10, div_pref = 12,
             kg_net = 14, sole_prop_inc = 18, sole_prop_loss = 20, part_scorp_inc = 22, part_scorp_loss = 24)
irs_row = function(label) {
  i = which(sheet[[1]] == label)
  stopifnot(length(i) == 1)
  v = suppressWarnings(as.numeric(sheet[i, IRS_COLS]))
  names(v) = names(IRS_COLS)
  v
}
irs = rbindlist(lapply(c('All returns, total', names(PCTILES)), function(l) as.data.table(as.list(irs_row(l)))[, slice := l]))
stopifnot(irs[slice == '0.001 percent', returns] == 1538)   # the published count


#------------------------------------------------------------------------------
# The TY2022 file, with and without the splice
#------------------------------------------------------------------------------

vdir = file.path(model_data_root(), 'Tax-Data/v1', VINTAGE, 'baseline')
emitted = fread(file.path(vdir, sprintf('tax_units_%d.csv', YEAR)))
stopifnot(!any(is.na(emitted$forbes_flag)), !any(is.na(emitted$dep_status)))
# Table 4.3 excludes dependent returns
with_splice = as.data.table(agi_proxy(as_tibble(emitted[filer == 1 & weight > 0 & dep_status == 0]), YEAR))
adj = as.data.table(readRDS(file.path(vdir, 'forbes_weight_adjustments.rds')))[year == YEAR]
stopifnot(!anyDuplicated(adj$id), all(adj$weight_factor > 0))
without_splice = copy(with_splice[forbes_flag == 0])
without_splice[adj, on = 'id', weight := weight / i.weight_factor]
rm(emitted)

# The Table 4.3 items from the file's columns. Net gain less loss is the 1040
# line after the loss limit (txbl_kg_proxy); Schedule C and partnership + S
# corporation split by the sign of the return's net, as the table does.
items = function(d) {
  d[, part_scorp_net := part_active + part_passive - part_active_loss - part_passive_loss - part_179 +
                        scorp_active + scorp_passive - scorp_active_loss - scorp_passive_loss - scorp_179]
  d[, .(id, weight, forbes_flag, agi = agi_proxy, wages, txbl_int, div = div_ord + div_pref, div_pref,
        kg_net = txbl_kg_proxy,
        sole_prop_inc = pmax(sole_prop, 0), sole_prop_loss = -pmin(sole_prop, 0),
        part_scorp_inc = pmax(part_scorp_net, 0), part_scorp_loss = -pmin(part_scorp_net, 0))]
}

# The weighted top p% of returns by AGI, the marginal record taken in part
top_slice = function(d, p) {
  o = d[order(-agi)][, cum_w := cumsum(weight)]
  n_top = p * sum(d$weight)
  o[, w_in := pmin(weight, pmax(n_top - (cum_w - weight), 0))]
  o[w_in > 0]
}

amount_cols = c('agi', 'wages', 'txbl_int', 'div', 'div_pref', 'kg_net',
                'sole_prop_inc', 'sole_prop_loss', 'part_scorp_inc', 'part_scorp_loss')
file_slices = function(d, label) {
  it = items(d)
  stopifnot(!any(is.na(it)))
  all_rows = it[, c(list(slice = 'All returns, total', returns = sum(weight), agi_floor = NA_real_,
                         forbes_returns = sum(weight[forbes_flag == 1])),
                    lapply(.SD, function(x) sum(x * weight) / 1e3)), .SDcols = amount_cols]
  tops = rbindlist(lapply(names(PCTILES), function(l) {
    s = top_slice(it, PCTILES[[l]])
    s[, c(list(slice = l, returns = sum(w_in), agi_floor = min(agi), forbes_returns = sum(w_in[forbes_flag == 1])),
          lapply(.SD, function(x) sum(x * w_in) / 1e3)), .SDcols = amount_cols]
  }))
  rbind(all_rows, tops)[, file := label]
}

file_tab = rbind(file_slices(with_splice, 'with'), file_slices(without_splice, 'without'))

# Long: one row per (slice, quantity), IRS beside with / without
long = melt(file_tab, id.vars = c('slice', 'file'), variable.name = 'quantity') %>%
  dcast(slice + quantity ~ file, value.var = 'value') %>%
  merge(melt(irs, id.vars = 'slice', variable.name = 'quantity', value.name = 'irs'),
        by = c('slice', 'quantity'), all.x = TRUE)
long[, `:=`(ratio_with = with / irs, ratio_without = without / irs)]
long[, slice := factor(slice, levels = c('All returns, total', names(PCTILES)))]
setorder(long, slice, quantity)

# Shares of the national total, the table's own headline
shares = long[quantity == 'agi' & slice != 'All returns, total',
              .(slice, irs_share = irs / long[quantity == 'agi' & slice == 'All returns, total', irs],
                with_share = with / long[quantity == 'agi' & slice == 'All returns, total', with],
                without_share = without / long[quantity == 'agi' & slice == 'All returns, total', without])]

fwrite(long,   file.path(RES, 'table43_2022.csv'))
fwrite(shares, file.path(RES, 'table43_2022_agi_shares.csv'))

options(width = 220)
cat(sprintf('TY%d: %d Forbes rows; %d records reweighted by the splice\n', YEAR, sum(with_splice$forbes_flag), nrow(adj)))
print(long[quantity %in% c('returns', 'agi_floor', 'forbes_returns'),
           .(slice, quantity, irs = round(irs), with = round(with), without = round(without))])
print(long[!quantity %in% c('returns', 'agi_floor', 'forbes_returns'),
           .(slice, quantity, irs_bn = round(irs / 1e6, 1), with_bn = round(with / 1e6, 1), without_bn = round(without / 1e6, 1),
             ratio_with = round(ratio_with, 3), ratio_without = round(ratio_without, 3))])
print(shares[, lapply(.SD, function(x) if (is.numeric(x)) round(100 * x, 3) else x)])
cat('TABLE43_2022_SUCCESS\n')
