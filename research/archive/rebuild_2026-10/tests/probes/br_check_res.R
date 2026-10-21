# Probe BR: check the residential property-type list against SCFP RESDBT and
# ORESRE (all residential other real estate, ignoring whether it yields income).
suppressPackageStartupMessages({library(haven); library(data.table)})
RAWDIR = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1'
RES_TYPES = c(12, 14, 21, 22, 25, 40, 41, 42, 43, 44, 49, 50, 52, 999)
WF = c(`2013` = 1.272833, `2016` = 1.233371)
for (wave in c(2013L, 2016L)) {
  yy = substr(as.character(wave), 3, 4)
  raw = read_dta(file.path(RAWDIR, wave, 'historical', sprintf('p%si6.dta', yy)),
                 col_select = c('y1', 'x42001', 'x1703', 'x1705', 'x1706', 'x1715', 'x1803', 'x1805', 'x1806', 'x1815', 'x2002', 'x2006'))
  d = as.data.table(lapply(raw, function(x) as.numeric(zap_labels(x))))
  setnames(d, tolower(names(d)))
  r1 = d$x1703 %in% RES_TYPES; r2 = d$x1803 %in% RES_TYPES
  debt = pmax(d$x1715, 0) * d$x1705 / 1e4 * r1 + pmax(d$x1815, 0) * d$x1805 / 1e4 * r2 + pmax(d$x2006, 0)
  val = pmax(d$x1706, 0) * d$x1705 / 1e4 * r1 + pmax(d$x1806, 0) * d$x1805 / 1e4 * r2 + pmax(d$x2002, 0)
  sc = fread(file.path(RAWDIR, wave, 'historical', sprintf('SCFP%d.csv', wave)), select = c('Y1', 'RESDBT', 'ORESRE'))
  m = merge(data.table(y1 = d$y1, w = d$x42001 / 5, debt, val), sc[, .(y1 = Y1, RESDBT, ORESRE)], by = 'y1')
  f = WF[as.character(wave)]
  cat(sprintf('wave %d: residential debt rebuilt / RESDBT %.4f; residential value rebuilt / ORESRE %.4f; type codes seen: %s\n',
              wave, sum(m$w * m$debt) / sum(m$w * m$RESDBT / f), sum(m$w * m$val) / sum(m$w * m$ORESRE / f),
              paste(sort(unique(c(d$x1703, d$x1803))), collapse = ',')))
}
