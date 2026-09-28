#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 07_top_tail_2022.R  (follow-up to 06) -- the top tail with and without Forbes
#
# create_2017_puf.R makes up the dollars of the PUF's four dropped aggregate
# records by scaling every record in its $10M+ group, which flattens the tail
# inside that class (06_puf_soi_gaps.R). From 2022 the Forbes splice appends
# US billionaires and, income-neutrally, takes their income off the top
# existing records. Does that restore the tail?
#
#   with     the TY2022 output as emitted (Forbes rows + splice weights)
#   without  Forbes rows dropped, splice weight factors divided back out
#            (forbes_weight_adjustments.rds, year 2022)
#   2015     the raw TY2015 PUF with and without the aggregate records, on
#            E00100 -- the tail shape the drop removed (nominal $, so compare
#            shares, not levels)
#
# Filers only (Table 1.4's universe); AGI is src/agi_proxy.R for 2022.
# Writes results/top_tail_2022/{soi_classes,tail_bins,top400}.csv.
#   sbatch --job-name=top_tail_2022 --mem=24G --time=01:00:00 \
#     --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_rscript.sbatch research/state_weights/filer_refit/07_top_tail_2022.R
#------------------------------------------------------------------------------

source('research/state_weights/filer_refit/tail_common.R')   # read_filers(), top_slice(), vintage_dir()
source('src/nonfilers/sw_soi.R')                             # irs_ind_aligned_path()

VINTAGE = '2026092720_filer_refit_v2'
YEAR    = 2022L
RES     = 'research/state_weights/filer_refit/results/top_tail_2022'
dir.create(RES, recursive = TRUE, showWarnings = FALSE)
TAIL_BREAKS = c(1e7, 2e7, 5e7, 1e8, 5e8, Inf)   # bins inside Table 1.4's $10M+ class
TOP_N = 400                                      # the IRS "400 highest AGI" cut
AGGREGATE_RECID_MIN = 999996


#------------------------------------------------------------------------------
# TY2022 file, with and without the splice
#------------------------------------------------------------------------------

with_splice = as.data.table(agi_proxy(as_tibble(read_filers(VINTAGE, YEAR)), YEAR))

adj = as.data.table(readRDS(file.path(vintage_dir(VINTAGE), 'forbes_weight_adjustments.rds')))[year == YEAR]
stopifnot(!anyDuplicated(adj$id), all(adj$id %in% with_splice$id), all(adj$weight_factor > 0))
without_splice = copy(with_splice[forbes_flag == 0])
without_splice[adj, on = 'id', weight := weight / i.weight_factor]

cat(sprintf('TY%d: %d Forbes rows; %d records reweighted by the splice\n',
            YEAR, sum(with_splice$forbes_flag), nrow(adj)))


#------------------------------------------------------------------------------
# Against Table 1.4 at the top classes
#------------------------------------------------------------------------------

# Table 1.4 series and the file columns that carry them
SOI_SERIES = list(
  'adjusted gross income less deficit' = function(d) d$agi_proxy,
  'wages'                              = function(d) d$wages,
  'taxable interest'                   = function(d) d$txbl_int,
  'ordinary dividends'                 = function(d) d$div_ord + d$div_pref,
  'sales of capital assets reported on form 1040, schedule d > taxable net gain' =
                                         function(d) pmax(d$kg_st + d$kg_lt, 0),
  'business or profession > net income' = function(d) pmax(d$sole_prop, 0))

soi = fread(irs_ind_aligned_path('panel_income_sources.csv'), na.strings = '')[
  tax_year == YEAR & panel == 'all returns' & !is.na(agi_lo) & agi_lo >= 1e6 &
  series %in% names(SOI_SERIES)]
# the panel carries AGI as measure 'value' (an amount, $ thousands), no count
soi[series == 'adjusted gross income less deficit' & measure == 'value', measure := 'amount']
stopifnot(!any(is.na(soi$value)), setequal(unique(soi$series), names(SOI_SERIES)),
          all(soi$measure %in% c('amount', 'returns')))
soi_breaks = sort(unique(soi$agi_lo))

file_classes = function(d, label) {
  top = d[agi_proxy >= min(soi_breaks)]
  top[, agi_lo := soi_breaks[findInterval(agi_proxy, soi_breaks)]]
  rbindlist(lapply(names(SOI_SERIES), function(s) {
    x = SOI_SERIES[[s]](top)
    stopifnot(!any(is.na(x)))
    data.table(agi_lo = top$agi_lo, x = x, w = top$weight)[
      , .(amount = sum(x * w) / 1e3, returns = sum((x != 0) * w)), by = agi_lo][
      , series := s]
  }))[, file := label]
}
soi_classes = rbind(file_classes(with_splice, 'with'), file_classes(without_splice, 'without')) %>%
  melt(id.vars = c('agi_lo', 'series', 'file'), variable.name = 'measure') %>%
  dcast(agi_lo + series + measure ~ file, value.var = 'value') %>%
  merge(soi[, .(agi_lo, series, measure, soi = value)], by = c('agi_lo', 'series', 'measure'))
soi_classes[, `:=`(ratio_with = with / soi, ratio_without = without / soi)]


#------------------------------------------------------------------------------
# Inside $10M+: bins, and the top 400
#------------------------------------------------------------------------------

tail_bins = function(agi, kg, w, label) {
  top = data.table(agi, kg, w)[agi >= TAIL_BREAKS[1]]
  top[, bin := TAIL_BREAKS[findInterval(agi, TAIL_BREAKS)]]
  top[, .(returns = sum(w), agi_bn = sum(agi * w) / 1e9, kg_bn = sum(kg * w) / 1e9), by = bin][
    order(bin)][, `:=`(agi_share = agi_bn / sum(agi_bn), kg_share = kg_bn / sum(kg_bn), file = label)]
}

# Weighted top N by AGI (tail_common.R's top_slice on agi = the proxy)
top_n = function(d, label) {
  in_top = top_slice(d[, .(agi = agi_proxy, weight, forbes_flag)], TOP_N)
  data.table(file = label,
             threshold_m     = min(in_top$agi) / 1e6,
             agi_bn          = sum(in_top$agi * in_top$w_in) / 1e9,
             share_all_agi   = sum(in_top$agi * in_top$w_in) / sum(d$agi_proxy * d$weight),
             forbes_returns  = in_top[forbes_flag == 1, sum(w_in)],
             records         = nrow(in_top))
}

kg_2022 = function(d) pmax(d$kg_st + d$kg_lt, 0)

puf_2015 = fread(file.path(raw_data_root(), 'IRS-PUF/v1/2023091310/historical/puf_2015.csv'),
                 select = c('RECID', 'S006', 'E00100', 'E01000'))
stopifnot(!any(is.na(puf_2015)), sum(puf_2015$RECID >= AGGREGATE_RECID_MIN) == 4)
puf_2015_noagg = puf_2015[RECID < AGGREGATE_RECID_MIN]

bins = rbind(
  tail_bins(with_splice$agi_proxy, kg_2022(with_splice), with_splice$weight, '2022 with splice'),
  tail_bins(without_splice$agi_proxy, kg_2022(without_splice), without_splice$weight, '2022 without splice'),
  tail_bins(puf_2015$E00100, pmax(puf_2015$E01000, 0), puf_2015$S006 / 100, '2015 PUF with aggregates'),
  tail_bins(puf_2015_noagg$E00100, pmax(puf_2015_noagg$E01000, 0), puf_2015_noagg$S006 / 100,
            '2015 PUF without aggregates'))

puf_2015_top = function(p, label) {
  top_n(data.table(agi_proxy = p$E00100, weight = p$S006 / 100, forbes_flag = 0L), label)
}
top400 = rbind(top_n(with_splice, '2022 with splice'), top_n(without_splice, '2022 without splice'),
               puf_2015_top(puf_2015, '2015 PUF with aggregates'),
               puf_2015_top(puf_2015_noagg, '2015 PUF without aggregates'))


fwrite(soi_classes, file.path(RES, 'soi_classes.csv'))
fwrite(bins,        file.path(RES, 'tail_bins.csv'))
fwrite(top400,      file.path(RES, 'top400.csv'))

options(width = 200)
print(soi_classes[agi_lo >= 2e6, .(agi_lo, series = substr(series, 1, 30), measure,
                                   soi = signif(soi, 4), ratio_with = round(ratio_with, 3),
                                   ratio_without = round(ratio_without, 3))])
print(bins[, .(file, bin, returns = round(returns), agi_bn = round(agi_bn, 1),
               agi_share = round(agi_share, 3), kg_share = round(kg_share, 3))])
print(top400)
cat('TOP_TAIL_2022_SUCCESS\n')
