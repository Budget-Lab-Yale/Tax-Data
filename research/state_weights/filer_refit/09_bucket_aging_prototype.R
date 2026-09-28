#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 09_bucket_aging_prototype.R -- percentile-specific value aging, off-pipeline
#
# The pipeline ages every income line by one scalar per (variable, year), so
# the within-variable shape is frozen at 2017 and the top percentiles drift
# from IRS Table 4.3 (08_table43.R). materialize() already supports a factor
# per (variable, year, bucket) -- wealth ages that way by DFA income bucket.
# This prototype applies the same idea to the income lines, on the v2 vintage's
# emitted files, without touching the pipeline:
#
#   bands    top 0.001%, 0.001-0.01%, 0.01-0.1%, 0.1-1% of non-dependent
#            filers by AGI proxy; the rest untouched
#   lines    the Table 4.3 items: wages, taxable interest, dividends, net
#            capital gain, Schedule C income / loss, partnership + S corp
#            income / loss
#   targets  GROWTH-relative: the file's own 2017 band total x the IRS band's
#            growth 2017 -> y. So the factors carry only the shape drift since
#            2017; what remains against IRS levels is the 2017 base gap carried
#            forward -- the part only base construction can fix.
#   solve    per (band, line) the factor is target / band sum. Two modes,
#            BUCKET_MODE=rerank (default) re-ranks membership on the adjusted
#            AGI and iterates -- which oscillates at the extreme band, records
#            at the margin flipping in and out (2022-23 did not settle in 12
#            rounds); BUCKET_MODE=frozen fixes each record's band on the
#            year's unadjusted AGI, the record_bucket.R convention wealth uses,
#            and solves in one pass. Weights are the vintage's.
#
# Also records the LEVEL factors the same bands would need to hit IRS outright,
# which sizes the base problem year by year.
#
# Writes results/bucket_aging/{comparison,factors,decomposition}.csv.
#   sbatch ... research/state_weights/filer_refit/run_bucket_aging.sbatch
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(readxl) })
source('src/nonfilers/sw_common.R')   # model_data_root()
source('src/agi_proxy.R')

VINTAGE  = '2026092720_filer_refit_v2'
BASE     = 2017L
YEARS    = 2018L:2023L
MODE     = Sys.getenv('BUCKET_MODE', unset = 'rerank')
stopifnot(MODE %in% c('rerank', 'frozen'))
RES      = file.path('research/state_weights/filer_refit/results/bucket_aging', MODE)
T43      = 'research/state_weights/filer_refit/results/table43'
IRS_FILE = c(`2017` = '17in03etr.xls', `2018` = '18in03etr.xls', `2019` = '19in43ts.xls',
             `2020` = '20in43ts.xls',  `2021` = '21in43ts.xls',  `2022` = '22in43ts.xls',
             `2023` = '23in43ts.xls')
SLICES   = c('0.001 percent' = 1e-5, '0.01 percent' = 1e-4, '0.1 percent' = 1e-3, '1 percent' = 1e-2)
BANDS    = c('top 0.001%', '0.001-0.01%', '0.01-0.1%', '0.1-1%')   # SLICES as non-cumulative bands
LINES    = c('wages', 'txbl_int', 'div', 'kg_net', 'sole_prop_inc', 'sole_prop_loss',
             'part_scorp_inc', 'part_scorp_loss')
MAX_ITER = 12
TOL      = 1e-4     # max |factor change| between iterations
dir.create(RES, recursive = TRUE, showWarnings = FALSE)
vdir = file.path(model_data_root(), 'Tax-Data/v1', VINTAGE, 'baseline')


#------------------------------------------------------------------------------
# IRS Table 4.3 by band (non-cumulative), $ thousands
#------------------------------------------------------------------------------

IRS_COLS = c(returns = 2, agi_floor = 3, agi = 4, wages = 6, txbl_int = 8, div = 10, div_pref = 12,
             kg_net = 14, sole_prop_inc = 18, sole_prop_loss = 20, part_scorp_inc = 22, part_scorp_loss = 24)
read_irs = function(year) {
  sheet = as.data.frame(read_excel(file.path(T43, IRS_FILE[[as.character(year)]]),
                                   col_names = FALSE, .name_repair = 'minimal'))
  labels = gsub('^\\.', '0.', gsub('\\s+', ' ', sheet[[1]]))
  rows = rbindlist(lapply(c('All returns, total', names(SLICES)), function(l) {
    i = which(labels == l); stopifnot(length(i) == 1)
    v = suppressWarnings(as.numeric(sheet[i, IRS_COLS])); names(v) = names(IRS_COLS)
    as.data.table(as.list(v))[, slice := l]
  }))
  rows[, c('sole_prop_loss', 'part_scorp_loss') := lapply(.SD, abs), .SDcols = c('sole_prop_loss', 'part_scorp_loss')]
  rows[, year := year]
}
irs_slices = rbindlist(lapply(c(BASE, YEARS), read_irs))

# cumulative slices -> bands
to_bands = function(sl, value_cols) {
  s = sl[slice %in% names(SLICES)][match(names(SLICES), slice)]
  b = copy(s)
  for (k in 4:2) b[k, (value_cols) := s[k, ..value_cols] - s[k - 1, ..value_cols]]
  b[, band := BANDS][, slice := NULL]
}
irs_bands = irs_slices[, to_bands(.SD, c('agi', LINES)), by = year]


#------------------------------------------------------------------------------
# The file: items, ranking, band sums
#------------------------------------------------------------------------------

# Columns the eight lines live in. Scaling a line scales every column it is
# built from, so the line's own sign split (income vs loss) is preserved.
LINE_COLS = list(
  wages           = c('wages', 'wages1', 'wages2'),
  txbl_int        = 'txbl_int',
  div             = c('div_ord', 'div_pref'),
  kg_net          = c('kg_st', 'kg_lt'),
  sole_prop_inc   = c('sole_prop', 'sole_prop1', 'sole_prop2'),
  sole_prop_loss  = c('sole_prop', 'sole_prop1', 'sole_prop2'),
  part_scorp_inc  = c('part_active', 'part_passive', 'part_active_loss', 'part_passive_loss', 'part_179',
                      'scorp_active', 'scorp_passive', 'scorp_active_loss', 'scorp_passive_loss', 'scorp_179'),
  part_scorp_loss = c('part_active', 'part_passive', 'part_active_loss', 'part_passive_loss', 'part_179',
                      'scorp_active', 'scorp_passive', 'scorp_active_loss', 'scorp_passive_loss', 'scorp_179'))
# which records a line's factor applies to (the sign split)
line_mask = function(d, line) {
  switch(line,
    sole_prop_inc   = d$sole_prop > 0,
    sole_prop_loss  = d$sole_prop < 0,
    part_scorp_inc  = d$part_scorp_net > 0,
    part_scorp_loss = d$part_scorp_net < 0,
    kg_net          = d$kg_st + d$kg_lt > 0,     # the loss side is capped at $3,000; left alone
    rep(TRUE, nrow(d)))
}

with_items = function(d, year) {
  d = as.data.table(agi_proxy(as_tibble(d), year))
  d[, part_scorp_net := part_active + part_passive - part_active_loss - part_passive_loss - part_179 +
                        scorp_active + scorp_passive - scorp_active_loss - scorp_passive_loss - scorp_179]
  d[, `:=`(agi = agi_proxy, div = div_ord + div_pref, kg_net = txbl_kg_proxy,
           sole_prop_inc = pmax(sole_prop, 0), sole_prop_loss = -pmin(sole_prop, 0),
           part_scorp_inc = pmax(part_scorp_net, 0), part_scorp_loss = -pmin(part_scorp_net, 0))]
  d
}

# band of each record: the weighted top p% by agi, marginal records to the
# upper band (a simplification of the fractional take in 08)
assign_bands = function(d) {
  o = order(-d$agi)
  cum = cumsum(d$weight[o]); N = sum(d$weight)
  band = rep(NA_character_, nrow(d))
  edges = c(0, SLICES * N)
  for (k in seq_along(BANDS)) band[o[cum > edges[k] & cum <= edges[k + 1]]] = BANDS[k]
  band
}

band_sums = function(d) {
  b = assign_bands(d)
  rbindlist(lapply(seq_along(BANDS), function(k) {
    m = !is.na(b) & b == BANDS[k]
    data.table(band = BANDS[k], returns = sum(d$weight[m]), agi_floor = min(d$agi[m]),
               as.data.table(as.list(sapply(c('agi', LINES), function(v) sum(d[[v]][m] * d$weight[m]) / 1e3))))
  }))
}

# cumulative slices from the file (as 08 reports them), $ thousands
slice_sums = function(d) {
  o = d[order(-agi)][, cum_w := cumsum(weight)]
  N = sum(d$weight)
  rbindlist(lapply(names(SLICES), function(l) {
    n_top = SLICES[[l]] * N
    o[, w_in := pmin(weight, pmax(n_top - (cum_w - weight), 0))]
    s = o[w_in > 0]
    data.table(slice = l, returns = sum(s$w_in), agi_floor = min(s$agi),
               as.data.table(as.list(sapply(c('agi', LINES), function(v) sum(s[[v]] * s$w_in) / 1e3))))
  }))
}

read_year = function(year) {
  e = fread(file.path(vdir, sprintf('tax_units_%d.csv', year)))
  stopifnot(!anyNA(e$dep_status))
  e[filer == 1 & weight > 0 & dep_status == 0]
}

#------------------------------------------------------------------------------
# 2017 base bands
#------------------------------------------------------------------------------

base_file  = with_items(read_year(BASE), BASE)
base_bands = band_sums(base_file)[, year := BASE]
base_slices = slice_sums(base_file)[, year := BASE]
rm(base_file)
cat(sprintf('mode: %s\n', MODE))
cat(sprintf('TY%d base: top band AGI $%.1fB (IRS $%.1fB)\n', BASE,
            base_bands[band == BANDS[1], agi] / 1e6, irs_bands[year == BASE & band == BANDS[1], agi] / 1e6))


#------------------------------------------------------------------------------
# Per year: growth-relative targets, iterate factors and membership
#------------------------------------------------------------------------------

run_year = function(year) {
  orig = read_year(year)
  cur  = with_items(orig, year)
  before_slices = slice_sums(cur)[, `:=`(year = year, file = 'v2')]
  before_bands  = band_sums(cur)[, year := year]

  yr = year   # a distinct name: inside [] `year` is the table's column
  # targets: file 2017 band total x IRS band growth
  g = merge(irs_bands[year == yr, c('band', LINES), with = FALSE],
            irs_bands[year == BASE,   c('band', LINES), with = FALSE], by = 'band', suffixes = c('', '_base'))
  tgt = copy(base_bands[, c('band', LINES), with = FALSE])
  for (v in LINES) tgt[, (v) := get(v) * g[[v]][match(tgt$band, g$band)] / g[[paste0(v, '_base')]][match(tgt$band, g$band)]]

  fac = CJ(band = BANDS, line = LINES)[, factor := 1]
  b_frozen = assign_bands(cur)   # membership on the year's unadjusted AGI
  for (it in seq_len(if (MODE == 'frozen') 2L else MAX_ITER)) {
    b = if (MODE == 'frozen') b_frozen else assign_bands(cur)
    new_fac = copy(fac)
    for (k in seq_along(BANDS)) for (v in LINES) {
      m = !is.na(b) & b == BANDS[k] & line_mask(cur, v)
      s = sum(cur[[v]][m] * cur$weight[m]) / 1e3
      f_old = fac[band == BANDS[k] & line == v, factor]
      t = tgt[band == BANDS[k]][[v]]
      f_new = if (s > 0 && is.finite(t) && t > 0) f_old * t / s else f_old
      new_fac[band == BANDS[k] & line == v, factor := f_new]
    }
    delta = max(abs(new_fac$factor - fac$factor))
    fac = new_fac
    # rebuild from the original values with the cumulative factors on current membership
    d = copy(orig)
    items0 = with_items(orig, year)   # sign masks from the ORIGINAL values
    for (k in seq_along(BANDS)) for (v in LINES) {
      m = !is.na(b) & b == BANDS[k] & line_mask(items0, v)
      f = fac[band == BANDS[k] & line == v, factor]
      if (f != 1 && any(m)) for (col in LINE_COLS[[v]]) d[m, (col) := get(col) * f]
    }
    cur = with_items(d, year)
    cat(sprintf('  TY%d iteration %d: max factor change %.5f\n', year, it, delta))
    if (delta < TOL) break
  }
  after_slices = slice_sums(cur)[, `:=`(year = year, file = 'v2 + bucket aging')]
  after_bands  = band_sums(cur)[, year := year]

  # level factors: what the bands would need to hit IRS outright, from here
  lvl = melt(after_bands[, c('band', LINES), with = FALSE], id.vars = 'band', variable.name = 'line', value.name = 'file') %>%
    merge(melt(irs_bands[year == yr, c('band', LINES), with = FALSE], id.vars = 'band', variable.name = 'line', value.name = 'irs'),
          by = c('band', 'line'))
  lvl[, `:=`(level_factor = irs / file, year = year)]

  list(slices = rbind(before_slices, after_slices), factors = fac[, year := year], level = lvl,
       bands = rbind(before_bands[, file := 'v2'], after_bands[, file := 'v2 + bucket aging']))
}

out = lapply(YEARS, run_year)
slices  = rbindlist(lapply(out, `[[`, 'slices'))
factors = rbindlist(lapply(out, `[[`, 'factors'))
level   = rbindlist(lapply(out, `[[`, 'level'))
bands   = rbindlist(lapply(out, `[[`, 'bands'))


#------------------------------------------------------------------------------
# Comparison against IRS, and the decomposition of the top-0.001% AGI gap
#------------------------------------------------------------------------------

comparison = melt(slices, id.vars = c('year', 'slice', 'file'), variable.name = 'quantity') %>%
  dcast(year + slice + quantity ~ file, value.var = 'value') %>%
  merge(melt(irs_slices[year %in% YEARS], id.vars = c('year', 'slice'), variable.name = 'quantity', value.name = 'irs'),
        by = c('year', 'slice', 'quantity'), all.x = TRUE)
setnames(comparison, c('v2', 'v2 + bucket aging'), c('before', 'after'))
comparison[, `:=`(ratio_before = before / irs, ratio_after = after / irs)]

# top 0.001% AGI: gap = (IRS - after) [base gap carried] + (after - before) [shape drift B removes]
decomp = comparison[slice == '0.001 percent' & quantity == 'agi',
                    .(year, irs_bn = irs / 1e6, before_bn = before / 1e6, after_bn = after / 1e6,
                      gap_before_bn = (irs - before) / 1e6, closed_by_B_bn = (after - before) / 1e6,
                      base_gap_carried_bn = (irs - after) / 1e6,
                      share_closed_by_B = (after - before) / (irs - before))]

fwrite(comparison, file.path(RES, 'comparison.csv'))
fwrite(factors,    file.path(RES, 'factors.csv'))
fwrite(level,      file.path(RES, 'level_factors.csv'))
fwrite(bands,      file.path(RES, 'bands.csv'))
fwrite(decomp,     file.path(RES, 'decomposition.csv'))

options(width = 220)
cat('\nTop 0.001% AGI: IRS / v2 / v2 + bucket aging ($B), and the gap decomposition\n')
print(decomp[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat('\nTop 0.001%: file / IRS by line, before -> after\n')
print(dcast(comparison[slice == '0.001 percent' & quantity %in% c('agi', 'agi_floor', LINES)],
            year ~ quantity, value.var = c('ratio_before', 'ratio_after'))[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x), by = year])
cat('\nTop 0.01% and 0.1%: AGI file / IRS, before -> after\n')
print(comparison[slice %in% c('0.01 percent', '0.1 percent', '1 percent') & quantity == 'agi',
                 .(year, slice, ratio_before = round(ratio_before, 3), ratio_after = round(ratio_after, 3))])
cat('\nGrowth-relative factors, top band\n')
print(dcast(factors[band == BANDS[1]], year ~ line, value.var = 'factor')[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x), by = year])
cat('\nLevel factors still needed after B, top band (IRS / file)\n')
print(dcast(level[band == BANDS[1]], year ~ line, value.var = 'level_factor')[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x), by = year])
cat('BUCKET_AGING_SUCCESS\n')
