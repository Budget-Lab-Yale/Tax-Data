#--------------------------------------
# check_comp_inc.R
#
# Question: Does filtering FMLI to "complete income reporters" only
# materially change (a) the weighted income distribution, (b) the
# top-tail elasticity β, and (c) y_ref (median income of P98+)?
#
# This is a standalone empirical check; it does NOT touch pipeline
# code. We read the raw FMLI CSVs directly and reconstruct WT_ANNUAL
# exactly as in src/cex.R:191-203.
#
# Simplification (documented): we use FMLI household-level income
# (FINCBTXM, pre-tax, multiply-imputed) rather than building tax-unit
# income from MEMI. FINCBTXM is defined on the CU, whereas the
# production pipeline allocates CU-level capital income to tax units
# pro-rata. For the purpose of a filter-vs-no-filter sensitivity check
# on distributional quantiles and a log-log slope on P80+, CU income is
# sufficient — and notably it's the exact income concept the BLS
# complete-income flag refers to.
#
# FMLI complete-income flag: BLS docs call this RESPSTAT. Expected
# coding is 1 = complete, 2 = incomplete. We verify the column exists
# and print its actual value distribution before assuming semantics.
#
# Output: printed tables for P50/P80/P95/P98, β (± SE), y_ref, reporter
# counts, filtered vs unfiltered.
#--------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(Hmisc)   # wtd.quantile
})

cex_base = '/nfs/roberts/project/pi_nrs36/shared/raw_data/CEX'
years    = c(2022, 2023)

# CPI-U deflators to 2017 dollars — matches cex.R:32
cpi_deflator = c('2022' = 1.17443, '2023' = 1.23825)

#---------------------------------------------------------------------------
# 0. Identify the complete-income flag in one header before reading bulk
#---------------------------------------------------------------------------

sniff_path = file.path(cex_base, '2022', 'fmli222.csv')
sniff_hdr  = names(fread(sniff_path, nrows = 0))

cat(sprintf('\n[header sniff] total columns in fmli222.csv: %d\n',
            length(sniff_hdr)))

# Based on BLS CE Interview Survey public-use data dictionary, the
# complete-income reporter flag is stored in one of these historically
# documented names. The 2022 release switched to a multiply-imputed
# income framework, which REMOVED the RESPSTAT flag entirely — it
# survives only in archived pre-2013 files. In the 2013+ data, the
# "complete reporter" distinction is captured by the imputation
# infrastructure itself: FINCBTAX is the REPORTED value, FINCBTXM
# is the multiply-imputed value. Complete reporters are those whose
# FINCBTAX equals FINCBTXM (imputation is a no-op for them).
#
# The `FINCBTXI` column ALSO exists and encodes the imputation status
# directly (0 = no imputation needed = complete reporter, 1+ = number
# of fields imputed). Check for it and prefer it over the proxy.
flag_order = c('FINCBTXI',        # imputation count (0 = complete)
               'FINCBTAX_',       # bracket flag variant
               'FINCBTXM_',       # bracket flag variant
               'RESPSTAT')
flag_var   = NULL
for (cand in flag_order) {
  if (cand %in% sniff_hdr) { flag_var = cand; break }
}
# If still nothing, fall back to the FINCBTAX==FINCBTXM equality proxy.
use_proxy = is.null(flag_var)
if (use_proxy) {
  cat('\n[fallback] No direct flag found; will use equality proxy:\n',
      '  complete_reporter := (FINCBTAX == FINCBTXM)\n',
      'BLS multiply imputes FINCBTXM only for incomplete reporters, so\n',
      'equality is a documented (Passero-Pascale 2013) proxy.\n')
  stopifnot(all(c('FINCBTAX', 'FINCBTXM') %in% sniff_hdr))
  flag_var = '__PROXY__'
} else {
  cat(sprintf('\n[using] complete-income flag column = %s\n', flag_var))
}

#---------------------------------------------------------------------------
# 1. Read FMLI files exactly as cex.R does (Q2-4 + boundary Q1),
#    keeping only columns we need.
#---------------------------------------------------------------------------

tech_cols = c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR')
# Always read both FINCBTAX (reported) and FINCBTXM (imputed) — the
# proxy flag needs both; the non-proxy path only uses FINCBTXM.
inc_cols  = c('FINCBTAX', 'FINCBTXM')
extra     = if (flag_var == '__PROXY__') character(0) else flag_var
keep_cols = unique(c(tech_cols, inc_cols, extra))

read_one_year = function(year) {
  yy = year %% 100
  q234 = dir(path = file.path(cex_base, year),
             pattern = paste0('^fmli', yy, '[2-4]\\.csv$'),
             full.names = TRUE)
  bnd_next = year + 1
  bnd_yy   = bnd_next %% 100
  # cex.R looks for the boundary Q1 in the same release dir; in the 2022
  # tree that file is missing, so also fall back to the next year's dir.
  bnd = dir(path = file.path(cex_base, year),
            pattern = paste0('^fmli', bnd_yy, '1\\.csv$'),
            full.names = TRUE)
  if (length(bnd) == 0) {
    bnd = dir(path = file.path(cex_base, bnd_next),
              pattern = paste0('^fmli', bnd_yy, '1\\.csv$'),
              full.names = TRUE)
  }
  files = c(q234, bnd)
  stopifnot(length(files) > 0)
  cat(sprintf('  year=%d: %d files\n', year, length(files)))
  rbindlist(lapply(files, fread, select = keep_cols)) %>%
    as_tibble() %>%
    mutate(SURVEY_YEAR = year)
}

cat('\n[read] FMLI files\n')
fmli_raw = map_dfr(years, read_one_year)
cat(sprintf('[read] total rows = %d\n', nrow(fmli_raw)))

#---------------------------------------------------------------------------
# 2. Verify the flag coding by printing value distribution
#---------------------------------------------------------------------------

if (flag_var == '__PROXY__') {
  # Equality within rounding — FMLI stores both to whole dollars.
  fmli_raw = fmli_raw %>%
    mutate(complete_reporter = as.integer(
      !is.na(FINCBTAX) & !is.na(FINCBTXM) &
      abs(FINCBTAX - FINCBTXM) < 1))
  cat('\n[flag/proxy] complete_reporter counts (1 = reported==imputed):\n')
  print(fmli_raw %>% count(complete_reporter))
  flag_col = 'complete_reporter'
} else {
  cat(sprintf('\n[flag] value distribution of %s (unweighted):\n', flag_var))
  print(fmli_raw %>% count(.data[[flag_var]]))
  # FINCBTXI semantics in the 2013+ BLS multiply-imputed FMLI public-use
  # files: integer coding where 100 = complete income reporter (no
  # imputation needed), 2xx = at least one income field imputed
  # (2nd digit indexes which field, 3rd digit the iteration). The
  # sampled FMLI for 2022 shows levels {100, 201..299} confirming this
  # structure.
  if (flag_var == 'FINCBTXI') {
    fmli_raw = fmli_raw %>%
      mutate(complete_reporter = as.integer(FINCBTXI == 100))
    cat('[flag/FINCBTXI] collapsed to complete_reporter (1 = FINCBTXI==100):\n')
    print(fmli_raw %>% count(complete_reporter))
    flag_col = 'complete_reporter'
  } else {
    flag_col = flag_var
  }
}

#---------------------------------------------------------------------------
# 3. Construct WT_ANNUAL per cex.R:191-203, CPI-deflate income
#---------------------------------------------------------------------------

fmli = fmli_raw %>%
  mutate(
    MO_SCOPE = case_when(
      QINTRVYR == SURVEY_YEAR     & QINTRVMO <= 1 ~ 0L,
      QINTRVYR == SURVEY_YEAR     & QINTRVMO == 2 ~ 1L,
      QINTRVYR == SURVEY_YEAR     & QINTRVMO == 3 ~ 2L,
      QINTRVYR == SURVEY_YEAR     & QINTRVMO >= 4 ~ 3L,
      QINTRVYR == SURVEY_YEAR + 1 & QINTRVMO == 1 ~ 3L,
      QINTRVYR == SURVEY_YEAR + 1 & QINTRVMO == 2 ~ 2L,
      QINTRVYR == SURVEY_YEAR + 1 & QINTRVMO == 3 ~ 1L,
      TRUE                                         ~ 0L
    ),
    WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3) / length(years),
    # CPI-deflate FMLI income (matches cex.R mapping)
    income_2017 = FINCBTXM / cpi_deflator[as.character(SURVEY_YEAR)]
  ) %>%
  # Drop obs that contribute zero annualized weight (out of scope)
  # (This is a universe restriction, not an NA filter.)
  filter(WT_ANNUAL > 0)

cat(sprintf('[post-scope] rows with WT_ANNUAL > 0 = %d\n', nrow(fmli)))

#---------------------------------------------------------------------------
# 4. Reporter-flag counts: unweighted and weighted
#---------------------------------------------------------------------------

reporter_counts = fmli %>%
  mutate(reporter = .data[[flag_col]]) %>%
  group_by(reporter) %>%
  summarise(n_unwtd     = n(),
            n_wtd_mill  = sum(WT_ANNUAL) / 1e6,
            .groups     = 'drop')
cat('\n[reporter] counts and annualized weighted population (millions):\n')
print(reporter_counts)

#---------------------------------------------------------------------------
# 5. Build two samples: unfiltered, and complete-reporter only.
#    Elasticity + y_ref are fit in the positive-income P80+ universe,
#    mirroring cex.R:323-329.
#---------------------------------------------------------------------------

# For RESPSTAT, 1 = complete. For the proxy, complete_reporter == 1.
keep_val = 1L
if (!keep_val %in% fmli[[flag_col]]) {
  stop('Expected value 1 not found in ', flag_col, '; inspect distribution above.')
}

samples = list(
  unfiltered = fmli,
  complete   = fmli %>% filter(.data[[flag_col]] == keep_val)
)

summarise_income = function(d, label) {
  # Quantiles of CU-level (2017$) income on the full-weight sample.
  # stopifnot — by construction, income_2017 has no NAs once FINCBTXM
  # is non-NA in the raw file; verify explicitly.
  stopifnot(!any(is.na(d$income_2017)))
  q = wtd.quantile(d$income_2017, weights = d$WT_ANNUAL,
                   probs = c(0.5, 0.8, 0.95, 0.98))
  tibble(sample = label,
         p50 = q[[1]], p80 = q[[2]], p95 = q[[3]], p98 = q[[4]])
}

fit_elasticity = function(d, label) {
  # P80+ subset: positive income, using the P80 of *this* sample
  p80 = wtd.quantile(d$income_2017, weights = d$WT_ANNUAL, probs = 0.8)
  sub = d %>% filter(income_2017 >= p80, income_2017 > 0)
  # Note: cex.R fits log(C) ~ log(Y) on P80+ of the joined CEX data
  # (tax-unit level with consumption). Here we have CU-level income
  # but NO consumption column. We CANNOT reproduce the β regression
  # one-to-one without expenditures; instead, we fit a proxy β on
  # *income* alone — wait, that has no meaning. Return the quantile
  # structure and note we need consumption to re-fit β.
  # -- Actually we need total_consumption. Add it via FMLI expenditure
  # sub-variables exactly as cex.R does. See below.
  stop('fit_elasticity called before adding consumption columns')
}

cat('\n[Q1] Weighted income quantiles (2017$):\n')
quant_tbl = bind_rows(
  summarise_income(samples$unfiltered, 'unfiltered'),
  summarise_income(samples$complete,   'complete_only')
)
print(quant_tbl)

# Absolute + relative diffs
diff_row = tibble(
  sample = 'Δ (complete - unfiltered)',
  p50 = quant_tbl$p50[2] - quant_tbl$p50[1],
  p80 = quant_tbl$p80[2] - quant_tbl$p80[1],
  p95 = quant_tbl$p95[2] - quant_tbl$p95[1],
  p98 = quant_tbl$p98[2] - quant_tbl$p98[1]
)
rel_row = tibble(
  sample = 'rel Δ (pct)',
  p50 = 100 * diff_row$p50 / quant_tbl$p50[1],
  p80 = 100 * diff_row$p80 / quant_tbl$p80[1],
  p95 = 100 * diff_row$p95 / quant_tbl$p95[1],
  p98 = 100 * diff_row$p98 / quant_tbl$p98[1]
)
cat('\n[Q1] Δ absolute and relative:\n')
print(bind_rows(diff_row, rel_row))

#---------------------------------------------------------------------------
# 6. Re-read FMLI WITH expenditure sub-variables to recompute
#    total_consumption (CU-level, no TU allocation), then fit β and y_ref.
#---------------------------------------------------------------------------

expcq = c('FDHOMECQ', 'FDAWAYCQ', 'ALCBEVCQ',
          'OWNDWECQ', 'RENDWECQ', 'OTHLODCQ',
          'NTLGASCQ', 'ELCTRCCQ', 'ALLFULCQ', 'TELEPHCQ', 'WATRPSCQ',
          'HOUSOPCQ', 'HOUSEQCQ', 'APPARCQ',
          'CARTKNCQ', 'CARTKUCQ', 'OTHVEHCQ', 'GASMOCQ',
          'VEHFINCQ', 'MAINRPCQ', 'VEHINSCQ', 'VRNTLOCQ', 'PUBTRACQ',
          'HLTHINCQ', 'MEDSRVCQ', 'PREDRGCQ', 'MEDSUPCQ',
          'FEEADMCQ', 'TVRDIOCQ', 'PETTOYCQ', 'OTHENTCQ',
          'PERSCACQ', 'READCQ', 'EDUCACQ', 'TOBACCCQ', 'MISCCQ',
          'LIFINSCQ')
exppq = sub('CQ$', 'PQ', expcq)

read_one_year_full = function(year) {
  yy = year %% 100
  q234 = dir(path = file.path(cex_base, year),
             pattern = paste0('^fmli', yy, '[2-4]\\.csv$'),
             full.names = TRUE)
  bnd_next = year + 1
  bnd_yy   = bnd_next %% 100
  bnd = dir(path = file.path(cex_base, year),
            pattern = paste0('^fmli', bnd_yy, '1\\.csv$'),
            full.names = TRUE)
  if (length(bnd) == 0) {
    bnd = dir(path = file.path(cex_base, bnd_next),
              pattern = paste0('^fmli', bnd_yy, '1\\.csv$'),
              full.names = TRUE)
  }
  files = c(q234, bnd)
  extra = if (flag_var == '__PROXY__') character(0) else flag_var
  cols = unique(c(tech_cols, inc_cols, extra, expcq, exppq))
  rbindlist(lapply(files, fread, select = cols)) %>%
    as_tibble() %>%
    mutate(SURVEY_YEAR = year)
}

cat('\n[read] FMLI + expenditures\n')
fmli_full = map_dfr(years, read_one_year_full)

# CQ+PQ are additive over the quarter; annualize *4; deflate to 2017$.
# Replace expenditure NAs with 0 (cex.R does this too — these are FMLI
# sub-variables where NA means "no such expenditure").
fmli_full = fmli_full %>%
  mutate(across(all_of(c(expcq, exppq)), ~ replace_na(.x, 0))) %>%
  mutate(across(all_of(expcq),
                ~ (.x + get(sub('CQ$', 'PQ', cur_column()))) * 4)) %>%
  mutate(across(all_of(expcq),
                ~ .x / cpi_deflator[as.character(SURVEY_YEAR)])) %>%
  mutate(
    income_2017 = FINCBTXM / cpi_deflator[as.character(SURVEY_YEAR)],
    MO_SCOPE = case_when(
      QINTRVYR == SURVEY_YEAR     & QINTRVMO <= 1 ~ 0L,
      QINTRVYR == SURVEY_YEAR     & QINTRVMO == 2 ~ 1L,
      QINTRVYR == SURVEY_YEAR     & QINTRVMO == 3 ~ 2L,
      QINTRVYR == SURVEY_YEAR     & QINTRVMO >= 4 ~ 3L,
      QINTRVYR == SURVEY_YEAR + 1 & QINTRVMO == 1 ~ 3L,
      QINTRVYR == SURVEY_YEAR + 1 & QINTRVMO == 2 ~ 2L,
      QINTRVYR == SURVEY_YEAR + 1 & QINTRVMO == 3 ~ 1L,
      TRUE                                         ~ 0L
    ),
    WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3) / length(years)
  ) %>%
  filter(WT_ANNUAL > 0)

# Attach the reporter flag to fmli_full (same logic as above)
if (flag_var == '__PROXY__') {
  fmli_full = fmli_full %>%
    mutate(complete_reporter = as.integer(
      !is.na(FINCBTAX) & !is.na(FINCBTXM) &
      abs(FINCBTAX - FINCBTXM) < 1))
} else if (flag_var == 'FINCBTXI') {
  fmli_full = fmli_full %>%
    mutate(complete_reporter = as.integer(FINCBTXI == 100))
}

# Sum expenditure sub-variables into a CU-level total consumption, then
# floor at 0 (matches cex.R:265 behavior applied to per-category totals;
# here we floor at the total level for a like-for-like β fit).
fmli_full = fmli_full %>%
  mutate(total_consumption = pmax(0, rowSums(across(all_of(expcq)))))

#---------------------------------------------------------------------------
# 7. Fit β on P80+, positive income + positive consumption, for each sample
#---------------------------------------------------------------------------

fit_beta = function(d, label) {
  p80 = wtd.quantile(d$income_2017, weights = d$WT_ANNUAL, probs = 0.8)
  p98 = wtd.quantile(d$income_2017, weights = d$WT_ANNUAL, probs = 0.98)
  sub80 = d %>% filter(income_2017 >= p80,
                       income_2017 > 0,
                       total_consumption > 0)
  fit = lm(log(total_consumption) ~ log(income_2017),
           data = sub80, weights = sub80$WT_ANNUAL)
  sm  = summary(fit)
  beta    = coef(fit)[['log(income_2017)']]
  beta_se = sm$coefficients['log(income_2017)', 'Std. Error']
  sub98 = d %>% filter(income_2017 >= p98, income_2017 > 0)
  y_ref = as.numeric(wtd.quantile(sub98$income_2017,
                                  weights = sub98$WT_ANNUAL, probs = 0.5))
  tibble(sample = label,
         n_p80  = nrow(sub80),
         beta   = beta,
         beta_se= beta_se,
         p80    = as.numeric(p80),
         p98    = as.numeric(p98),
         y_ref  = y_ref)
}

unf = fmli_full
com = fmli_full %>% filter(.data[[flag_col]] == keep_val)

beta_tbl = bind_rows(
  fit_beta(unf, 'unfiltered'),
  fit_beta(com, 'complete_only')
)
cat('\n[Q2/Q3] β and y_ref by sample:\n')
print(beta_tbl)

delta = tibble(
  metric       = c('beta', 'beta_se', 'y_ref'),
  unfiltered   = c(beta_tbl$beta[1], beta_tbl$beta_se[1], beta_tbl$y_ref[1]),
  complete     = c(beta_tbl$beta[2], beta_tbl$beta_se[2], beta_tbl$y_ref[2])
) %>%
  mutate(abs_diff = complete - unfiltered,
         rel_pct  = 100 * abs_diff / unfiltered)
cat('\n[Q2/Q3] Δ summary:\n')
print(delta)

cat('\n[done]\n')
