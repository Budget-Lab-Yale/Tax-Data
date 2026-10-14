# Test 2 piece A: which dollars is each SCFP summary file in? Compares SCFP
# fields with the raw interview items they are built from, record by record,
# and SCFP WGT with raw X42001. Writes $S/data/scf_units/scfp_dollar_factors.csv
suppressPackageStartupMessages({library(dplyr); library(data.table); library(haven)})
out = list()
for (wave in c(2013, 2016, 2019, 2022)) {
  yy = substr(wave, 3, 4)
  d = file.path('/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1', wave, 'historical')
  raw = read_dta(file.path(d, sprintf('p%si6.dta', yy)), col_select = any_of(c('y1','Y1','x716','X716','x604','X604','x614','X614','x623','X623','x42001','X42001','x5702','X5702','x413','X413')))
  names(raw) = toupper(names(raw))
  scfp = fread(file.path(d, sprintf('SCFP%d.csv', wave))); setnames(scfp, toupper(names(scfp)))
  j = inner_join(as_tibble(raw), as_tibble(scfp)[, c('Y1','HOUSES','WAGEINC','WGT')], by = 'Y1')
  h = j %>% filter(X716 > 0, X604 == 0, X614 == 0, X623 == 0)
  r_h = h$HOUSES / h$X716
  r_w = with(j[j$X5702 > 0, ], WAGEINC / X5702)
  out[[length(out) + 1]] = data.frame(wave = wave, rows = nrow(j),
    wealth_factor_median = median(r_h), wealth_factor_p01 = quantile(r_h, .01), wealth_factor_p99 = quantile(r_h, .99),
    income_factor_median = median(r_w), income_factor_min = min(r_w), income_factor_max = max(r_w),
    weight_ratio_median = median(j$WGT / j$X42001), weight_rows_differ = sum(abs(j$WGT - j$X42001) > 1e-6 * j$X42001),
    weight_total_scfp_over_raw = sum(j$WGT) / sum(j$X42001))
}
res = bind_rows(out); print(res)
fwrite(res, '/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/scf_units/scfp_dollar_factors.csv')
