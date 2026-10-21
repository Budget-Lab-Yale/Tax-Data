# 00_explore.R
#
# Checks on the SCF summary extracts (SCFP) before the pooled-donor test:
#   - implicate structure: rows per household, implicate numbering;
#   - how WGT sums across the five implicates (compare to published
#     household counts);
#   - dollar basis: SCFP INCOME against raw X5729 (total income, nominal,
#     prior calendar year), and SCFP WGT against raw X42001;
#   - missing values and negative values in the variables the test uses;
#   - sample sizes in the cells and income groups the test uses.
# Runs under Slurm only (run_explore.sh).

suppressPackageStartupMessages({ library(data.table); library(haven) })

root  = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1'
waves = c(2013, 2016, 2019, 2022)
vars  = c('YY1', 'Y1', 'WGT', 'AGE', 'AGECL', 'MARRIED', 'KIDS', 'INCOME',
          'WAGEINC', 'HOUSES', 'STOCKS', 'NMMF', 'RETQLIQ', 'BUS', 'HBUS',
          'DEBT', 'NETWORTH', 'ASSET')

wq = function(x, w, p) { o = order(x); cw = cumsum(w[o]) / sum(w); x[o][which(cw >= p)[1]] }

for (y in waves) {
  cat('\n==================== ', y, ' ====================\n')
  d = fread(file.path(root, y, 'historical', sprintf('SCFP%d.csv', y)), select = vars)
  cat('rows', nrow(d), ' households', uniqueN(d$YY1), '\n')
  imp = d$Y1 - 10 * d$YY1
  cat('implicate numbers:\n'); print(table(imp))
  cat('rows per household:\n'); print(table(table(d$YY1)))
  cat(sprintf('sum WGT over all rows: %.0f   / 5: %.0f\n', sum(d$WGT), sum(d$WGT) / 5))
  cat('sum WGT by implicate:\n'); print(round(tapply(d$WGT, imp, sum)))
  # Is WGT constant across a household's implicates?
  sdw = d[, .(sd = sd(WGT), m = mean(WGT)), by = YY1]
  cat(sprintf('households whose WGT varies across implicates: %d\n', sum(sdw$sd > 1e-6 * sdw$m)))
  cat('NA counts:\n'); print(colSums(is.na(d)))
  for (v in c('INCOME', 'WAGEINC', 'HOUSES', 'STOCKS', 'NMMF', 'RETQLIQ', 'BUS', 'DEBT', 'NETWORTH')) {
    cat(sprintf('%-9s min %14.0f  max %14.0f  n<0 %5d  n==0 %5d  wtd mean %12.0f  wtd median %10.0f\n',
                v, min(d[[v]]), max(d[[v]]), sum(d[[v]] < 0), sum(d[[v]] == 0),
                weighted.mean(d[[v]], d$WGT), wq(d[[v]], d$WGT, 0.5)))
  }
  cat('HBUS vs BUS!=0 :\n'); print(table(HBUS = d$HBUS, BUSnz = d$BUS != 0))
  cat('AGECL x MARRIED (rows):\n'); print(table(d$AGECL, d$MARRIED))
  cat('KIDS:\n'); print(table(pmin(d$KIDS, 4)))
  # Income groups: households (rows / 5) by weighted income percentile
  o = order(d$INCOME); cw = cumsum(d$WGT[o]) / sum(d$WGT); pct = numeric(nrow(d)); pct[o] = cw
  g = cut(pct, c(0, .2, .4, .6, .8, .9, .99, 1), include.lowest = TRUE)
  cat('rows by income group (all implicates; divide by 5 for households):\n'); print(table(g))
  cat('income percentiles 5,10,50,90,99:', sapply(c(.05, .1, .5, .9, .99), function(p) wq(d$INCOME, d$WGT, p)), '\n')

  # Raw file: weights and income in nominal dollars
  raw = file.path(root, y, 'historical', sprintf('p%02di6.dta', y %% 100))
  nm  = names(read_dta(raw, n_max = 0))
  want = c('y1', 'yy1', 'x42001', 'x5729', 'x8022')
  sel = nm[tolower(nm) %in% want]
  r = as.data.table(read_dta(raw, col_select = all_of(sel)))
  setnames(r, tolower(names(r)))
  cat('raw rows', nrow(r), ' sum x42001', round(sum(r$x42001)), ' /5', round(sum(r$x42001) / 5), '\n')
  m = merge(d[, .(Y1, WGT, INCOME, AGE)], r, by.x = 'Y1', by.y = 'y1')
  cat('matched rows', nrow(m), '\n')
  cat('WGT / x42001 quantiles:\n'); print(quantile(m$WGT / m$x42001, c(0, .01, .5, .99, 1)))
  pos = m$x5729 > 1000
  cat('INCOME / x5729 (x5729 > 1000) quantiles:\n'); print(quantile(m$INCOME[pos] / m$x5729[pos], c(0, .01, .25, .5, .75, .99, 1)))
  cat('AGE == x8022 share:', mean(m$AGE == m$x8022), '\n')
}
