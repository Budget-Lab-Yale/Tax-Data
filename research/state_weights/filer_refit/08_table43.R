#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 08_table43.R  (follow-up to 07) -- the top percentiles against IRS Table 4.3,
# TY2017-2023
#
# Pub 1304 Table 4.3 tabulates returns excluding dependents by descending
# cumulative AGI percentile -- top 0.001%, 0.01%, 0.1%, 1%, ... -- with the
# count, the AGI floor, AGI and income items. It is the one published table
# that looks inside the $10M+ class Table 1.4 stops at, so it measures the
# compressed tail directly (research/state_weights/notes/top_tail_backcast.md).
#
#   with     the year's output as emitted (Forbes rows + splice weights from
#            TY2022; before that there is no splice and "with" is the file)
#   without  TY2022+: Forbes rows dropped, splice weight factors divided out
#
# Filers excluding dependent returns (Table 4.3's universe); AGI is
# src/agi_proxy.R; each slice is the weighted top p% of OUR returns, so the
# counts compare our universe to the IRS's and the floor compares tails.
#
# The IRS sheets (results/table43/, downloaded 2026-09-28; compute nodes may
# lack internet): TY2019-2023 are {yy}in43ts.xls from
# https://www.irs.gov/statistics/soi-tax-stats-individual-statistical-tables-by-tax-rate-and-income-percentile;
# TY2017-2018 are the same table under its earlier name, Table 3 of the
# "tax rates and tax shares" release, {yy}in03etr.xls (first 24 columns
# identical). Labels read ".001 percent" through TY2020 and "0.001 percent"
# after; TY2023 prints the loss columns as negatives.
#
# Writes results/table43/table43_2017_2023.csv (long: year, slice, quantity,
# irs, with, without) and table43_agi_shares.csv.
#   sbatch ... research/state_weights/filer_refit/run_table43.sbatch
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(readxl) })
source('src/nonfilers/sw_common.R')   # model_data_root()
source('src/agi_proxy.R')

VINTAGE = '2026092720_filer_refit_v2'
YEARS   = 2017L:2023L
RES     = 'research/state_weights/filer_refit/results/table43'
IRS_FILE = c(`2017` = '17in03etr.xls', `2018` = '18in03etr.xls', `2019` = '19in43ts.xls',
             `2020` = '20in43ts.xls',  `2021` = '21in43ts.xls',  `2022` = '22in43ts.xls',
             `2023` = '23in43ts.xls')
PCTILES = c('0.001 percent' = 1e-5, '0.01 percent' = 1e-4, '0.1 percent' = 1e-3, '1 percent' = 1e-2)
SPLICE_YEARS = 2022L:2025L   # build_forbes_splice(years = ...) in main.R

vdir = file.path(model_data_root(), 'Tax-Data/v1', VINTAGE, 'baseline')


#------------------------------------------------------------------------------
# IRS Table 4.3: the top-percentile rows, $ thousands (the floor in dollars)
#------------------------------------------------------------------------------

# Column layout, identical TY2017-2023 in these 24 columns (verified
# 2026-09-28): label; returns; AGI floor; AGI; then (returns, amount) pairs for
# wages, taxable interest, ordinary dividends, qualified dividends, net capital
# gain less loss, gains at preferential rates, Schedule C net income, Schedule
# C net loss, partnership and S corporation net income, their net loss.
IRS_COLS = c(returns = 2, agi_floor = 3, agi = 4, wages = 6, txbl_int = 8, div = 10, div_pref = 12,
             kg_net = 14, sole_prop_inc = 18, sole_prop_loss = 20, part_scorp_inc = 22, part_scorp_loss = 24)
LOSS_COLS = c('sole_prop_loss', 'part_scorp_loss')

read_irs = function(year) {
  sheet = as.data.frame(read_excel(file.path(RES, IRS_FILE[[as.character(year)]]),
                                   col_names = FALSE, .name_repair = 'minimal'))
  title = gsub('\\s+', ' ', sheet[1, 1])
  stopifnot(grepl('Excluding Dependents', title), grepl(paste('Tax Year', year), title))
  labels = gsub('^\\.', '0.', gsub('\\s+', ' ', sheet[[1]]))       # ".001 percent" -> "0.001 percent"
  rows = rbindlist(lapply(c('All returns, total', names(PCTILES)), function(l) {
    i = which(labels == l)
    stopifnot(length(i) == 1)
    v = suppressWarnings(as.numeric(sheet[i, IRS_COLS]))
    names(v) = names(IRS_COLS)
    as.data.table(as.list(v))[, slice := l]
  }))
  rows[, (LOSS_COLS) := lapply(.SD, abs), .SDcols = LOSS_COLS]   # TY2023 prints losses negative
  stopifnot(rows[slice == 'All returns, total', returns] > 1.4e8,
            rows[slice == '0.001 percent', returns] %between% c(1400, 1600))
  rows[, year := year]
}
irs = rbindlist(lapply(YEARS, read_irs))


#------------------------------------------------------------------------------
# The file, per year, with and without the splice
#------------------------------------------------------------------------------

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

file_year = function(year) {
  emitted = fread(file.path(vdir, sprintf('tax_units_%d.csv', year)))
  stopifnot(!any(is.na(emitted$forbes_flag)), !any(is.na(emitted$dep_status)))
  # Table 4.3 excludes dependent returns
  with_splice = as.data.table(agi_proxy(as_tibble(emitted[filer == 1 & weight > 0 & dep_status == 0]), year))
  rm(emitted)
  out = file_slices(with_splice, 'with')
  if (year %in% SPLICE_YEARS) {
    yr = year   # a distinct name: inside [] `year` is the table's column
    adj = as.data.table(readRDS(file.path(vdir, 'forbes_weight_adjustments.rds')))[year == yr]
    stopifnot(nrow(adj) > 0, !anyDuplicated(adj$id), all(adj$weight_factor > 0))
    without_splice = copy(with_splice[forbes_flag == 0])
    without_splice[adj, on = 'id', weight := weight / i.weight_factor]
    out = rbind(out, file_slices(without_splice, 'without'))
  } else {
    stopifnot(sum(with_splice$forbes_flag) == 0)
  }
  message(sprintf('TY%d: %d Forbes rows', year, sum(with_splice$forbes_flag)))
  out[, year := year]
}
file_tab = rbindlist(lapply(YEARS, file_year))

# Long: one row per (year, slice, quantity), IRS beside with / without
long = melt(file_tab, id.vars = c('year', 'slice', 'file'), variable.name = 'quantity') %>%
  dcast(year + slice + quantity ~ file, value.var = 'value') %>%
  merge(melt(irs, id.vars = c('year', 'slice'), variable.name = 'quantity', value.name = 'irs'),
        by = c('year', 'slice', 'quantity'), all.x = TRUE)
long[, `:=`(ratio_with = with / irs, ratio_without = without / irs)]
long[, slice := factor(slice, levels = c('All returns, total', names(PCTILES)))]
setorder(long, year, slice, quantity)

# Shares of the national total, the table's own headline
nat = long[quantity == 'agi' & slice == 'All returns, total', .(year, irs_nat = irs, with_nat = with, without_nat = without)]
shares = merge(long[quantity == 'agi' & slice != 'All returns, total', .(year, slice, irs, with, without)], nat, by = 'year')[
  , .(year, slice, irs_share = irs / irs_nat, with_share = with / with_nat, without_share = without / without_nat)]

fwrite(long,   file.path(RES, 'table43_2017_2023.csv'))
fwrite(shares, file.path(RES, 'table43_agi_shares.csv'))

options(width = 220)
cat('\nTop 0.001%: returns, AGI floor ($M), AGI ($B) -- IRS / with / without\n')
print(long[slice == '0.001 percent' & quantity %in% c('returns', 'agi_floor', 'agi'),
           .(year, quantity, irs = signif(irs, 4), with = signif(with, 4), without = signif(without, 4),
             ratio_with = round(ratio_with, 3), ratio_without = round(ratio_without, 3))])
cat('\nAGI shares (%), by slice\n')
print(dcast(shares, year ~ slice, value.var = c('irs_share', 'with_share'))[, lapply(.SD, function(x) round(100 * x, 2)), by = year])
cat('\nTop 0.001% income items, file with splice / IRS\n')
print(dcast(long[slice == '0.001 percent' & quantity %in% amount_cols], year ~ quantity, value.var = 'ratio_with')[
  , lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat('\nTop 0.01% income items, file with splice / IRS\n')
print(dcast(long[slice == '0.01 percent' & quantity %in% amount_cols], year ~ quantity, value.var = 'ratio_with')[
  , lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat('\nAll returns, file / IRS\n')
print(dcast(long[slice == 'All returns, total' & quantity %in% c('returns', amount_cols)], year ~ quantity, value.var = 'ratio_with')[
  , lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat('TABLE43_SUCCESS\n')
