# 00b_dollar_basis.R: ratio of SCFP STOCKS (directly held stocks) to raw X3915
# (nominal market value of directly held stocks), per wave, to find the price
# adjustment the Fed applied to balance-sheet items in these extracts. Also
# tabulates the SCFP holding flags against amount > 0.
suppressPackageStartupMessages({ library(data.table); library(haven) })
root = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1'
for (y in c(2013, 2016, 2019, 2022)) {
  d = fread(file.path(root, y, 'historical', sprintf('SCFP%d.csv', y)),
            select = c('Y1', 'STOCKS', 'HSTOCKS', 'NMMF', 'HNMMF', 'HOUSES', 'HHOUSES',
                       'RETQLIQ', 'HRETQLIQ', 'DEBT', 'HDEBT', 'BUS', 'HBUS'))
  raw = file.path(root, y, 'historical', sprintf('p%02di6.dta', y %% 100))
  nm = names(read_dta(raw, n_max = 0))
  r = as.data.table(read_dta(raw, col_select = all_of(nm[tolower(nm) %in% c('y1', 'x3915')])))
  setnames(r, tolower(names(r)))
  m = merge(d, r, by.x = 'Y1', by.y = 'y1')
  p = m$x3915 > 1000
  cat(y, 'STOCKS / x3915 quantiles:', round(quantile(m$STOCKS[p] / m$x3915[p], c(0, .01, .5, .99, 1)), 4), '\n')
  for (v in c('STOCKS', 'NMMF', 'HOUSES', 'RETQLIQ', 'DEBT', 'BUS')) {
    f = d[[paste0('H', v)]]
    cat(sprintf('  %-8s flag=1 & amt>0 %6d  flag=1 & amt<=0 %5d  flag=0 & amt>0 %5d  flag=0 & amt<0 %5d\n', v,
                sum(f == 1 & d[[v]] > 0), sum(f == 1 & d[[v]] <= 0), sum(f == 0 & d[[v]] > 0), sum(f == 0 & d[[v]] < 0)))
  }
}
