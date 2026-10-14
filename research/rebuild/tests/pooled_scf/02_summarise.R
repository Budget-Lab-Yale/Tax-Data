# 02_summarise.R
#
# Collects the per-split results of 01_pooled_scf_test.R and writes means and
# standard deviations across random splits. Runs under Slurm only.
suppressPackageStartupMessages(library(data.table))
IN  = '/nfs/roberts/scratch/pi_nrs36/jar335/pooled_scf/results'
OUT = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/pooled_scf'
fs  = list.files(IN, pattern = '^res_.*\\.rds$', full.names = TRUE)
L   = lapply(fs, readRDS)
res = rbindlist(lapply(L, `[[`, 'res'), fill = TRUE)
tru = rbindlist(lapply(L, `[[`, 'truth'))
cat('files:', length(fs), '\n'); print(tru[, .N, by = target])
cat('non-converged calibrations:', sum(!res$conv), '\n')
if (any(!res$conv)) print(res[conv == FALSE, .(target, split, donors, M, footing, targets, prec, gmax, miss_max)])
cat('largest final gradient among calibrations:', max(res$gmax, na.rm = TRUE), '\n')

# Errors on the untargeted shares, relative to each split's own truth.
res = merge(res, tru[, .(target, split, t_top1 = top1, t_top10 = top10, t_own_stock = own_stock,
                         t_stock_bus = stock_bus)], by = c('target', 'split'))
res[, `:=`(top1_err = top1 - t_top1, top10_err = top10 - t_top10,
           own_stock_err = own_stock - t_own_stock, stock_bus_err = stock_bus - t_stock_bus)]

keys = c('target', 'donors', 'M', 'footing', 'targets', 'prec', 'stage')
num  = setdiff(names(res)[sapply(res, is.numeric)], c(keys, 'split'))
mean_tab = res[, lapply(.SD, mean), by = keys, .SDcols = num]
sd_tab   = res[, lapply(.SD, sd),   by = keys, .SDcols = num]
fwrite(mean_tab, file.path(OUT, 'results_mean.csv'))
fwrite(sd_tab,   file.path(OUT, 'results_sd.csv'))
fwrite(res,      file.path(dirname(IN), 'results_by_split.csv'))   # large: kept in scratch
fwrite(tru[, lapply(.SD, mean), by = target, .SDcols = setdiff(names(tru), c('target', 'split'))],
       file.path(OUT, 'truth_mean.csv'))

options(width = 260, digits = 3, datatable.print.nrows = 500, datatable.print.class = FALSE)
cols_acc = c('miss_max', 'miss_med', 'z_max', 'z_mean', 'dec_nw_err', 'dec_tot_err', 'dec_rate_err', 'age_nw_err', 'age_tot_err',
             'age_rate_err', 'top1_err', 'top10_err', 'own_stock_err', 'stock_bus_err',
             'ratio_med_err', 'ratio_p90_err', 'energy')
cols_w = c('miss_max', 'rec_max', 'rec_min', 'rec_out', 'ess_rec', 'copy_ess', 'target_wave_share',
           'nearest_wave_share')
for (t in sort(unique(res$target), decreasing = TRUE)) {
  cat('\n\n######## target', t, '########\n')
  print(tru[target == t, lapply(.SD, mean), .SDcols = c('top1', 'top10', 'own_stock', 'stock_bus', 'n_rec', 'n_rec_hh', 'tt_dist', 'ed_floor')])
  cvs = Reduce(`+`, lapply(L[sapply(L, function(x) x$truth$target == t)], `[[`, 'cv')) / sum(sapply(L, function(x) x$truth$target == t))
  cat('\nBootstrap CV of targets (mean over splits), rows = income groups:\n'); print(round(cvs, 3))
  m = mean_tab[target == t]; s = sd_tab[target == t]
  cat('\n-- accuracy, means over splits --\n'); print(m[, c(keys[-1], cols_acc), with = FALSE])
  cat('\n-- accuracy, SD over splits --\n');    print(s[, c(keys[-1], cols_acc), with = FALSE])
  cat('\n-- weights, means over splits --\n');  print(m[, c(keys[-1], cols_w), with = FALSE])
}
