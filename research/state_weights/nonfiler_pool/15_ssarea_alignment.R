#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 15_ssarea_alignment.R  (decision S21, superseding the S19 handoff-only scale)
#
# The partition of adults on ONE universe, for EVERY built year:
#
#   filing adults + pool adults + claimed-dependent netting + not-in-universe
#                                                             = ssArea adults
#
# per age band, where ssArea is CBO's Social Security area population -- the
# universe of Macro-Projections' married_{age}/unmarried_{age} cells that
# Tax-Data carries every weight forward on (SSA Trustees' Report counts before
# CBO's first projected year, CBO after; verified equal to the person).
#
# What changed from S19 (2026-08-30 -> S21, JI 2026-09-11):
#
#   * ONE universe, every year (S21a). S19 put only T = 2023 on ssArea and left
#     2017-2022 on the Census PEP resident basis, which made the non-filer count
#     step +15% at the handoff year, about half of it the universe switch.
#   * Out-of-state filers stay filers (S21b). S15 removed the "Other Areas"
#     returns (APO/FPO, citizens abroad) and PR from the filing level because
#     they are outside PEP. They are INSIDE ssArea, so on this universe they
#     are filing adults. The qualifying-surviving-spouse double-count
#     correction, which is about return arithmetic rather than geography,
#     stays.
#   * The remaining wedge is NAMED, not filled (S21c). ssArea minus PEP minus
#     the filers outside PEP is Puerto Rico, the territories and other citizens
#     abroad who do not file: a not-in-universe block for a federal
#     income-tax model (IRC 933), carried in the identity as a number and never
#     appended as records. The pool therefore keeps its PEP calibration and NO
#     scale is applied at emit.
#
# The block is MEASURED as the closing residual of the identity and CHECKED
# against its expected composition (ssArea - PEP - filers outside PEP). The
# two agree by algebra when the pool closed its anchor and the pro-rata
# conventions match, so the gap is a consistency assertion, not a result.
#
# WHAT THE BLOCK CONTAINS -- measured 2026-09-11, and it is not only geography.
# Macro-Projections' cells are a JANUARY 1 population (SSA Trustees' Report
# series before CBO's first projected year), while the anchors' PEP is Census's
# JULY 1 estimate, so "ssArea - PEP" by band carries six months of cohort
# aging (65+ is ~0.9M smaller on January 1 than the following July 1; 18-25
# likewise) and any difference between the two series' estimation vintages,
# on top of the definitional wedge (Puerto Rico, the territories, citizens
# abroad). In 2017 the block is NEGATIVE in three bands for that reason; by
# 2023 it is positive in every band. So it is published with a decomposition:
# `block_timing` approximates the Jan-1-vs-Jul-1 piece as the change in PEP
# between the previous and current July (half a year's cohort flow) and
# `block_universe` is the remainder -- the part a PR/territories/abroad
# population series would have to explain. Positivity is ASSERTED only at the
# handoff year, the one whose partition the projection scales forward; other
# years report it. Separating the definitional piece properly needs a Jan-1
# resident series or an explicit PR/territories adult series -- open follow-up.
#
# Conventions, stated:
#   * T1.6 assigns a joint return's two adults to the PRIMARY's band; MFJ and
#     MFS adults are married, single and HoH unmarried; QSS rides inside the
#     MFJ block and is treated married (~0.08M, immaterial at cell scale).
#   * The QSS correction and the filers-outside-PEP count are spread PRO RATA
#     across bands by published filing adults, as the anchor spreads them.
#   * Filers outside PEP are taken as (published - anchor's corrected level -
#     QSS), i.e. exactly what the pool was calibrated against, so the identity
#     closes for a year whose anchor CARRIED the correction (2023, built before
#     HT2 TY2023 landed). The HT2-measured figure is reported beside it.
#   * Claimed adult dependents net out as UNMARRIED in their own band.
#   * The band x married split is published, not asserted (T1.6's primary-band
#     convention makes it unidentifiable; see the S19 note in the log).
#
# Products, per year:
#   ssarea_partition_{year}.csv  the band partition with every component
#   ssarea_wedge_{year}.csv      PEP vs ssArea residual per band (informational)
# and ssarea_partition_summary.csv across years.
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/15_ssarea_alignment.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(stringr); library(readxl); library(yaml); library(readr)
})
source('src/nonfilers/state_weights.R')

RES   <- 'research/state_weights/nonfiler_pool/results'
ANCH  <- 'research/state_weights/nonfiler_residual/results'
BANDS <- c('18_25', '26_34', '35_44', '45_54', '55_64', '65p')
YEARS <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)))
if (!length(YEARS)) YEARS <- 2017:2023

# Calibration closes the anchor to ~1e-8 adults and the emit gate to 1 adult;
# the identity gap inherits both plus floating summation order.
GAP_TOL <- 10
# The year whose partition Tax-Data scales forward (FILER_OBS_LAST there). The
# block must be positive in every band here; elsewhere it is reported.
HANDOFF_YEAR <- 2023L

# The demography the partition is built on. Same vintage 14_ uses; Tax-Data's
# own pin is read too and the cells compared, so a divergence is announced.
MACRO_VINTAGE <- c(model = 'Macro-Projections', version = 'v3', vintage = '2026071916')
read_cells <- function(vintage) {
  d <- model_data_path(MACRO_VINTAGE[['model']], MACRO_VINTAGE[['version']],
                       vintage, 'baseline')
  macro <- rbindlist(list(
    as.data.table(read_csv(file.path(d, 'historical.csv'),  show_col_types = FALSE)),
    as.data.table(read_csv(file.path(d, 'projections.csv'), show_col_types = FALSE))
  ), fill = TRUE)
  cells <- melt(macro[, c('year', grep('^(un)?married_[0-9]+$', names(macro),
                                       value = TRUE)), with = FALSE],
                id.vars = 'year', variable.name = 'k', value.name = 'n')
  cells[, `:=`(age     = as.integer(sub('.*_', '', as.character(k))),
               married = !startsWith(as.character(k), 'un'))]
  cells[!is.na(n), .(year, age, married, n)]
}
cells <- read_cells(MACRO_VINTAGE[['vintage']])

taxdata_pin <- tryCatch(
  yaml::read_yaml('config/runscripts/baseline.yaml')$dependency_info$`Macro-Projections`$vintage,
  error = function(e) NULL)
if (!is.null(taxdata_pin) && as.character(taxdata_pin) != MACRO_VINTAGE[['vintage']]) {
  other <- read_cells(as.character(taxdata_pin))
  cmp <- merge(cells[age >= 18 & year %in% YEARS, .(a = sum(n)), by = year],
               other[age >= 18 & year %in% YEARS, .(b = sum(n)), by = year], by = 'year')
  message(sprintf(paste('Macro-Projections vintages differ: this script %s,',
                        'Tax-Data pins %s. Adult cells %s for %s.'),
                  MACRO_VINTAGE[['vintage']], taxdata_pin,
                  if (cmp[, all(abs(a - b) < 1)]) 'IDENTICAL' else
                    sprintf('DIFFER by up to %.3f%%', 100 * cmp[, max(abs(a / b - 1))]),
                  paste(range(YEARS), collapse = '-')))
}

summary_rows <- list()

for (yr in YEARS) {
  message('=== TY', yr)

  #---------------------------------------------------------------------------
  # ssArea adults by band x married
  #---------------------------------------------------------------------------
  ss <- cells[year == yr & age >= 18,
              .(ss_adults = sum(n)),
              by = .(band = as.character(a16_band(age)), married)]
  stopifnot(setequal(unique(ss$band), BANDS))

  #---------------------------------------------------------------------------
  # Filing adults: T1.6 published, less the QSS double-count only (S21b)
  #---------------------------------------------------------------------------
  t16 <- read_pub1304_t16(yr)
  fa <- t16[block != 'all' & band != 'u18',
            .(filing_adults = sum(n_returns * fifelse(block == 'mfj', 2, 1))),
            by = .(band, married = block %in% c('mfj', 'mfs'))]
  u18 <- t16[block == 'all' & band == 'u18', sum(n_returns)]
  fa[band == '18_25' & married == FALSE, filing_adults := filing_adults - u18]
  fa_pub <- fa[, .(filing_adults_published = sum(filing_adults)), by = band]
  pub_total <- fa_pub[, sum(filing_adults_published)]

  ht2 <- read_ht2(ht2_path(yr), yr)
  fp  <- ht2_filing_persons(ht2)
  oo  <- ht2_filing_persons(ht2, states = NONTAX_BUCKETS)
  mfs_qss <- fp[, sum(mfs_qss_returns)] + (if (nrow(oo)) oo[, sum(mfs_qss_returns)] else 0)
  qss <- mfs_qss - t16[block == 'mfs', sum(n_returns)]
  stopifnot(qss >= 0)
  fa_oos_measured <- if (nrow(oo)) oo[, sum(married_filing_adults + single_filing_adults)] else 0

  # What the pool was calibrated against: the anchor's corrected filing level.
  # published - corrected = out-of-state + QSS as the anchor applied them
  # (measured, or carried as a fraction for a year without HT2 at build time).
  anchor <- fread(file.path(ANCH, sprintf('national_anchor_%d.csv', yr)))
  anchor_fa <- anchor[band != 'total_18p', sum(filing_adults)]
  corr_total <- pub_total - anchor_fa
  fa_oos_effective <- corr_total - qss
  stopifnot(fa_oos_effective > 0)

  fa_pub[, share := filing_adults_published / pub_total]
  fa_pub[, `:=`(qss_correction      = qss * share,
                filing_adults       = filing_adults_published - qss * share,
                filing_outside_pep  = fa_oos_effective * share)]

  #---------------------------------------------------------------------------
  # The pool's side: emitted adults (as 05 builds them) and the netting
  #---------------------------------------------------------------------------
  st  <- readRDS(file.path(RES, sprintf('units_%d.rds', yr)))
  u   <- readRDS(file.path(RES, sprintf('calibrated_units_%d.rds', yr)))
  gqp <- fread(file.path(RES, sprintf('gq_persons_%d.csv.gz', yr)))
  hh_pre <- u[unit_type == 'nondependent' & age_head >= 18 & weight * (1 - p_file_cal) > 0,
              .(band    = as.character(a16_band(pmax(18L, as.integer(age_head)))),
                married = filing_status == 'joint',
                adults  = weight * (1 - p_file_cal) * (1 + (filing_status == 'joint')))]
  gq_pre <- gqp[!is.na(p_file) & p_file < 1 & AGE >= 18,
                .(band    = as.character(a16_band(pmax(18L, as.integer(AGE)))),
                  married = FALSE,
                  adults  = PERWT * (1 - p_file))]
  emit <- rbind(hh_pre, gq_pre)[, .(emitted = sum(adults)), by = .(band, married)]

  deps <- st$persons[is_dependent == TRUE & AGE >= 18,
                     .(SERIAL, PERNUM, AGE, ASECWT,
                       unit_id = as.numeric(SERIAL) * 100 + PERNUM + 1e9)]
  deps <- merge(deps, u[unit_type == 'dependent', .(unit_id, p_file_cal)],
                by = 'unit_id', all.x = TRUE)
  deps[is.na(p_file_cal), p_file_cal := 0]
  net <- deps[, .(netting = sum(ASECWT * (1 - p_file_cal))),
              by = .(band = as.character(a16_band(AGE)))]

  #---------------------------------------------------------------------------
  # The partition: the not-in-universe block closes it; its composition is
  # asserted against ssArea - PEP - filers outside PEP
  #---------------------------------------------------------------------------
  by_band <- function(d, v) d[, setNames(.(sum(get(v))), v), by = band]
  cells_y <- Reduce(function(a, b) merge(a, b, by = 'band', all = TRUE),
                    list(by_band(ss, 'ss_adults'),
                         anchor[band != 'total_18p', .(band, pep_adults)],
                         fa_pub[, .(band, filing_adults_published, qss_correction,
                                    filing_adults, filing_outside_pep)],
                         by_band(emit, 'emitted'),
                         net))
  stopifnot(!anyNA(cells_y), nrow(cells_y) == length(BANDS))

  cells_y[, not_in_universe := ss_adults - filing_adults - emitted - netting]
  cells_y[, niu_expected    := ss_adults - pep_adults - filing_outside_pep]
  cells_y[, niu_gap         := not_in_universe - niu_expected]

  # Timing decomposition: PEP is July 1, the cells are January 1. Half of the
  # July-to-July change in PEP by band approximates the January-1 PEP, so the
  # timing piece of the block is (PEP_jan1 - PEP_jul1) = -(PEP_y - PEP_{y-1})/2.
  prev <- fread(file.path(ANCH, sprintf('national_anchor_%d.csv', yr - 1L)))
  cells_y <- merge(cells_y, prev[band != 'total_18p', .(band, pep_prev = pep_adults)],
                   by = 'band')
  cells_y[, block_timing   := -(pep_adults - pep_prev) / 2]
  cells_y[, block_universe := not_in_universe - block_timing]
  cells_y[, pep_prev := NULL]

  mar <- merge(ss[married == TRUE, .(band, ss_married = ss_adults)],
               fa[married == TRUE, .(band, filing_married = filing_adults)],
               by = 'band')
  cells_y <- merge(cells_y, mar, by = 'band')

  stopifnot(all(abs(cells_y$niu_gap) < GAP_TOL),
            all(abs(cells_y[, filing_adults + emitted + netting + not_in_universe -
                              ss_adults]) < 1e-6))
  neg <- cells_y[not_in_universe <= 0, band]
  if (yr == HANDOFF_YEAR) {
    stopifnot(length(neg) == 0)
  } else if (length(neg)) {
    message(sprintf('  NOTE: not-in-universe block is negative in %s -- the Jan-1 vs',
                    paste(neg, collapse = ', ')),
            ' Jul-1 timing and series-vintage pieces outweigh the definitional wedge',
            ' there (see block_timing / block_universe)')
  }

  fwrite(cells_y[match(BANDS, band)],
         file.path(RES, sprintf('ssarea_partition_%d.csv', yr)))

  wedge <- cells_y[, .(band, pep_adults, ss_adults,
                       residual_pep    = pep_adults - (filing_adults - filing_outside_pep),
                       residual_ssarea = emitted + netting + not_in_universe)]
  wedge[, `:=`(adults_wedge_pct   = 100 * (ss_adults / pep_adults - 1),
               residual_wedge_pct = 100 * (residual_ssarea / residual_pep - 1))]
  fwrite(wedge[match(BANDS, band)], file.path(RES, sprintf('ssarea_wedge_%d.csv', yr)))

  tot <- cells_y[, lapply(.SD, sum), .SDcols = c('ss_adults', 'pep_adults',
                   'filing_adults_published', 'qss_correction', 'filing_adults',
                   'filing_outside_pep', 'emitted', 'netting', 'not_in_universe',
                   'block_timing', 'block_universe', 'niu_gap')]
  summary_rows[[as.character(yr)]] <- cbind(
    data.table(year = yr, fa_oos_measured_ht2 = fa_oos_measured), tot)

  message(sprintf(paste('  ssArea %.2fM = filing %.2fM (published %.2fM - QSS %.3fM)',
                        '+ pool %.2fM + netting %.2fM + not-in-universe %.2fM'),
                  tot$ss_adults / 1e6, tot$filing_adults / 1e6,
                  tot$filing_adults_published / 1e6, tot$qss_correction / 1e6,
                  tot$emitted / 1e6, tot$netting / 1e6, tot$not_in_universe / 1e6))
  message(sprintf(paste('  block composition: ssArea - PEP %.2fM, less filers outside',
                        'PEP %.2fM (anchor-effective; HT2 measures %.2fM) -> gap %+.1f adults;',
                        'of the block, Jan-1/Jul-1 timing %+.2fM, universe+vintage %+.2fM'),
                  (tot$ss_adults - tot$pep_adults) / 1e6, tot$filing_outside_pep / 1e6,
                  fa_oos_measured / 1e6, tot$niu_gap,
                  tot$block_timing / 1e6, tot$block_universe / 1e6))
  message('  wrote ssarea_partition_', yr, '.csv, ssarea_wedge_', yr, '.csv')
}

summary <- rbindlist(summary_rows)
fwrite(summary, file.path(RES, 'ssarea_partition_summary.csv'))
message('\nnot-in-universe block by year (M adults; universe+vintage part in brackets): ',
        paste(sprintf('%d %.2f [%.2f]', summary$year, summary$not_in_universe / 1e6,
                      summary$block_universe / 1e6), collapse = ' | '))
message('pool adults by year (M): ',
        paste(sprintf('%d %.2f', summary$year, summary$emitted / 1e6), collapse = ' | '))
message('wrote ssarea_partition_summary.csv')
