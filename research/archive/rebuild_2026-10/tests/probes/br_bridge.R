# Probe BR: household-level bridge amounts from the SCF raw files, waves 2013
# and 2016, one row per household x implicate (y1), in survey-year dollars.
#   mortint_raw  interest on home and second-home loans: balance x annual rate
#   mortint_ded  the same limited as 2015 law limited it ($1m purchase debt,
#                $100k home-equity debt)
#   retax        real estate taxes on the home, annualized (X721 x X722)
#   charity      charitable contributions of the family (X5823)
# Checks: home loans and home-equity lines rebuilt here against the SCFP
# summary variables (NH_MORT, HELOC, RESDBT), item codes, NA counts.
# Writes $Q/scf_items/bridge_hh_<wave>.csv.
suppressPackageStartupMessages({library(haven); library(dplyr); library(data.table)})
RAWDIR = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1'
Q = '/nfs/roberts/scratch/pi_nrs36/jar335/probes/br/scf_items'
dir.create(Q, recursive = TRUE, showWarnings = FALSE)
WEALTH_FACTOR = c(`2013` = 1.272833, `2016` = 1.233371)   # SCFP files are in 2022 dollars (test 2)
# Residential property types in the Fed's bulletin macro for ORESRE / RESDBT.
RES_TYPES = c(12, 14, 21, 22, 25, 40, 41, 42, 43, 44, 49, 50, 52, 999)
freq_per_year = function(f) {
  m = c(`2` = 52, `3` = 26, `4` = 12, `5` = 4, `6` = 1, `11` = 2, `12` = 6, `31` = 24)
  out = unname(m[as.character(f)])
  ifelse(is.na(out), 1, out)        # other codes (annual or not stated): taken as annual; counted below
}
pos = function(x) pmax(x, 0)        # SCF -1 = "nothing", other negatives are codes, never amounts
for (wave in c(2013L, 2016L)) {
  yy = substr(as.character(wave), 3, 4)
  raw = read_dta(file.path(RAWDIR, wave, 'historical', sprintf('p%si6.dta', yy)))
  names(raw) = tolower(names(raw))
  need = c('y1', 'x42001', 'x721', 'x722', 'x805', 'x816', 'x905', 'x916', 'x1005', 'x1016', 'x1044', 'x1045',
           'x1103', 'x1108', 'x1111', 'x1114', 'x1119', 'x1122', 'x1125', 'x1130', 'x1133', 'x1136',
           'x1703', 'x1705', 'x1715', 'x1726', 'x1729', 'x1803', 'x1805', 'x1815', 'x1826', 'x1829',
           'x2006', 'x5822', 'x5823', 'x7660', 'x716')
  miss = setdiff(need, names(raw)); if (length(miss)) stop('missing items: ', paste(miss, collapse = ' '))
  d = as.data.table(lapply(raw[, need], function(x) as.numeric(zap_labels(x))))
  na = sapply(d, function(x) sum(is.na(x)))
  if (any(na > 0)) { print(na[na > 0]); stop('NA in SCF items') }
  w = d$x42001 / 5
  cat(sprintf('\n===== wave %d: %d household-implicate rows, %.1f m households\n', wave, nrow(d), sum(w) / 1e6))
  wt = function(m) sprintf('%.1f%%', 100 * sum(w[m]) / sum(w))
  # --- codes
  cat('X722 frequency codes among X721 > 0 (weighted share):\n'); print(round(tapply(w[d$x721 > 0], d$x722[d$x721 > 0], sum) / sum(w[d$x721 > 0]), 4))
  cat('X721 negative codes:', paste(unique(d$x721[d$x721 < 0]), collapse = ','), ' share', wt(d$x721 < 0), '\n')
  for (r in c('x816', 'x916', 'x1016', 'x1045', 'x1111', 'x1122', 'x1133', 'x1726', 'x1826'))
    cat(sprintf('%s negative codes: %s (rows %d)\n', r, paste(unique(d[[r]][d[[r]] < 0]), collapse = ','), sum(d[[r]] < 0)))
  cat('X5822 codes:'); print(round(tapply(w, d$x5822, sum) / sum(w), 4))
  ch = d$x5823[d$x5823 > 0]; chw = w[d$x5823 > 0]
  cat(sprintf('X5823 positive: min %g; share of households %s; quantiles of positives (unweighted) %s\n', min(ch), wt(d$x5823 > 0),
              paste(quantile(ch, c(.01, .05, .1, .5, .9, .99)), collapse = ' ')))
  cat(sprintf('X5823 share of positives below 500: %.4f; exactly 500: %.4f\n', sum(chw[ch < 500]) / sum(chw), sum(chw[ch == 500]) / sum(chw)))
  # --- loans on the home (purchase debt)
  home_bal = pos(d$x805) + pos(d$x905) + pos(d$x1005) + pos(d$x1044)
  home_int = pos(d$x805) * pos(d$x816) / 1e4 + pos(d$x905) * pos(d$x916) / 1e4 +
             pos(d$x1005) * pos(d$x1016) / 1e4 + pos(d$x1044) * pos(d$x1045) / 1e4
  # --- home-equity lines in use (secured by the home: X1103/X1114/X1125 = 1)
  heq_bal = pos(d$x1108) * (d$x1103 == 1) + pos(d$x1119) * (d$x1114 == 1) + pos(d$x1130) * (d$x1125 == 1)
  heq_int = pos(d$x1108) * (d$x1103 == 1) * pos(d$x1111) / 1e4 + pos(d$x1119) * (d$x1114 == 1) * pos(d$x1122) / 1e4 +
            pos(d$x1130) * (d$x1125 == 1) * pos(d$x1133) / 1e4
  # --- second homes: residential other real estate with no income from it
  #     (a property with rental income goes on Schedule E, not Schedule A)
  re1 = d$x1703 %in% RES_TYPES; re2 = d$x1803 %in% RES_TYPES
  sh1 = pos(d$x1705) / 1e4; sh2 = pos(d$x1805) / 1e4
  vac1 = re1 & d$x1729 != 1; vac2 = re2 & d$x1829 != 1
  ln_rate = c(d$x1726[pos(d$x1715) > 0 & d$x1726 > 0], d$x1826[pos(d$x1815) > 0 & d$x1826 > 0])
  ln_w = c(w[pos(d$x1715) > 0 & d$x1726 > 0], w[pos(d$x1815) > 0 & d$x1826 > 0])
  mean_rate = sum(ln_rate * ln_w) / sum(ln_w)
  cat(sprintf('other real estate loans: weighted mean rate %.0f (hundredths of a percent), used for X2006\n', mean_rate))
  vac_bal = pos(d$x1715) * sh1 * vac1 + pos(d$x1815) * sh2 * vac2 + pos(d$x2006)
  vac_int = pos(d$x1715) * sh1 * vac1 * pos(d$x1726) / 1e4 + pos(d$x1815) * sh2 * vac2 * pos(d$x1826) / 1e4 +
            pos(d$x2006) * mean_rate / 1e4
  # --- 2015 law limit: interest on at most $1m of purchase debt and $100k of home-equity debt
  acq_bal = home_bal + vac_bal; acq_int = home_int + vac_int
  ded = acq_int * pmin(1, 1e6 / pmax(acq_bal, 1)) + heq_int * pmin(1, 1e5 / pmax(heq_bal, 1))
  cat(sprintf('households with purchase debt over $1m: %s; with home-equity debt over $100k: %s\n', wt(acq_bal > 1e6), wt(heq_bal > 1e5)))
  # --- real estate taxes on the home
  f722 = freq_per_year(d$x722)
  cat('X722 codes taken as annual (not in the frequency map), weighted share of X721 > 0:',
      sprintf('%.4f', sum(w[d$x721 > 0 & !(d$x722 %in% c(2, 3, 4, 5, 6, 11, 12, 31))]) / sum(w[d$x721 > 0])), '\n')
  retax = pos(d$x721) * f722
  charity = pos(d$x5823)
  out = data.table(y1 = d$y1, mortint_raw = acq_int + heq_int, mortint_ded = ded, retax = retax, charity = charity,
                   home_bal = home_bal, heq_bal = heq_bal, vac_bal = vac_bal, home_int = home_int, heq_int = heq_int,
                   vac_int = vac_int)
  fwrite(out, file.path(Q, sprintf('bridge_hh_%d.csv', wave)))
  # --- checks against SCFP (2022 dollars) and totals
  sc = fread(file.path(RAWDIR, wave, 'historical', sprintf('SCFP%d.csv', wave)))
  setnames(sc, toupper(names(sc)))
  m = merge(out, sc[, .(y1 = Y1, NH_MORT, HELOC, RESDBT, HOUSES)], by = 'y1')
  stopifnot(nrow(m) == nrow(out))
  wm = d$x42001[match(m$y1, d$y1)] / 5
  f = WEALTH_FACTOR[as.character(wave)]
  cat(sprintf('check: home purchase loans here / SCFP NH_MORT (survey-year $): %.4f\n', sum(wm * m$home_bal) / sum(wm * m$NH_MORT / f)))
  cat(sprintf('check: home-equity lines here / SCFP HELOC: %.4f\n', sum(wm * m$heq_bal) / sum(wm * m$HELOC / f)))
  cat(sprintf('check: second-home loans here / SCFP RESDBT (all residential other real estate debt): %.4f\n', sum(wm * m$vac_bal) / sum(wm * m$RESDBT / f)))
  cat(sprintf('check: record-level max abs diff home loans vs NH_MORT/f: %.1f\n', max(abs(m$home_bal - m$NH_MORT / f))))
  tot = function(x) sum(w * x) / 1e9
  cat(sprintf('totals ($bn, survey-year dollars, all households): interest before limit %.1f, after limit %.1f (home %.1f, home equity %.1f, second homes %.1f); real estate tax %.1f; charity %.1f\n',
              tot(acq_int + heq_int), tot(ded), tot(home_int), tot(heq_int), tot(vac_int), tot(retax), tot(charity)))
  cat(sprintf('shares of households: interest > 0 %s; real estate tax > 0 %s; charity > 0 %s; home value > 0 %s; home owners with no tax %s\n',
              wt(ded > 0), wt(retax > 0), wt(charity > 0), wt(d$x716 > 0), wt(d$x716 > 0 & retax == 0)))
  rr = ifelse(home_bal > 0, home_int / home_bal, NA)
  cat('implied average rate on home loans (households with a loan), weighted quantiles:',
      paste(round(quantile(rep(rr[!is.na(rr)], 1), c(.1, .5, .9)), 4), collapse = ' '), '\n')
}
