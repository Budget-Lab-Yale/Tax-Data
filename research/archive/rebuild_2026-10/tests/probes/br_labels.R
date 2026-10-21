# Probe BR: list SCF raw-file variable labels matching mortgage, interest rate,
# property tax, charity; print weighted summaries of candidate items.
suppressPackageStartupMessages({library(haven); library(dplyr)})
RAWDIR = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1'
for (wave in c(2016L, 2013L)) {
  yy = substr(as.character(wave), 3, 4)
  raw = read_dta(file.path(RAWDIR, wave, 'historical', sprintf('p%si6.dta', yy)))
  labs = sapply(raw, function(x) { l = attr(x, 'label'); if (is.null(l)) '' else l })
  cat(sprintf('\n===== wave %d: %d rows, %d columns; labelled: %d\n', wave, nrow(raw), ncol(raw), sum(labs != '')))
  pat = 'MORT|RATE|INTEREST|TAX|CHARIT|CONTRIB|DONAT|GIFT|PAYMENT|PROPERTY|LAND CONTRACT|HOME|RESIDENCE|VACATION|REAL ESTATE|HELOC|LINE OF CREDIT'
  hit = which(grepl(pat, toupper(labs)))
  for (i in hit) cat(sprintf('%s\t%s\n', names(raw)[i], labs[i]))
  sc = read.csv(file.path(RAWDIR, wave, 'historical', sprintf('SCFP%d.csv', wave)), nrows = 5)
  cat('\nSCFP columns:\n'); print(names(sc))
}
