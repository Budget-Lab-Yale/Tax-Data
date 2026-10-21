# Probe BR: extract the SCF raw items on the home, mortgages, lines of
# credit, other real estate and charity (household x implicate rows), with
# their labels, for waves 2013 and 2016, to scratch. Prints labels and
# weighted summaries of each kept item.
suppressPackageStartupMessages({library(haven); library(dplyr); library(data.table)})
RAWDIR = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1'
Q = '/nfs/roberts/scratch/pi_nrs36/jar335/probes/br/scf_items'
dir.create(Q, recursive = TRUE, showWarnings = FALSE)
rng = function(a, b) paste0('x', a:b)
for (wave in c(2013L, 2016L)) {
  yy = substr(as.character(wave), 3, 4)
  raw = read_dta(file.path(RAWDIR, wave, 'historical', sprintf('p%si6.dta', yy)))
  names(raw) = tolower(names(raw))
  want = c('y1', 'yy1', 'x42001', rng(500, 530), rng(600, 630), rng(700, 1140), rng(1200, 1220),
           rng(1700, 1730), rng(1800, 1830), rng(1900, 1930), rng(2000, 2012), rng(8400, 8420),
           rng(5820, 5826), rng(7130, 7140), 'x7660', 'x5744', 'x5746', 'x6437')
  keep = intersect(want, names(raw))
  d = raw[, keep]
  labs = sapply(d, function(x) { l = attr(x, 'label'); if (is.null(l)) '' else l })
  d = as.data.table(lapply(d, function(x) as.numeric(zap_labels(x))))
  fwrite(d, file.path(Q, sprintf('items_%d.csv.gz', wave)))
  fwrite(data.table(var = names(labs), label = labs), file.path(Q, sprintf('labels_%d.csv', wave)))
  w = d$x42001 / 5
  cat(sprintf('\n===== wave %d: %d rows, %d kept columns, weighted households %.1f m\n', wave, nrow(d), ncol(d), sum(w) / 1e6))
  for (v in setdiff(keep, c('y1', 'yy1', 'x42001'))) {
    x = d[[v]]
    nz = x != 0
    if (!any(nz)) next
    q = quantile(x[nz], c(0, .1, .5, .9, 1), names = FALSE)
    cat(sprintf('%-7s %-55s nonzero %5.1f%%  neg %4.1f%%  min %g p10 %g p50 %g p90 %g max %g  distinct %d\n',
                v, substr(labs[[v]], 1, 55), 100 * sum(w[nz]) / sum(w), 100 * sum(w[x < 0]) / sum(w),
                q[1], q[2], q[3], q[4], q[5], length(unique(x))))
  }
}
