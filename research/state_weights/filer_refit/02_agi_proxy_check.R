#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 02_agi_proxy_check.R  (Phase 3b step 3a, S40)
#
# Scores src/agi_proxy.R -- the AGI the filer refit classes records by --
# against Tax-Simulator's computed AGI on the same records, record by record,
# for TY2017, 2019 and 2023. The benchmark is the phase0_s33 baseline run
# (Tax-Simulator v1), whose static detail files carry agi, txbl_ss, txbl_kg,
# sch_e and above_ded per record id, on Tax-Data vintage
# 2026092220_s33_scratch (its dependencies.csv). E00100 is no benchmark: it is
# the source PUF's AGI, never aged (create_2017_puf.R rescales components only).
#
# Tiers scored:
#   total_income  the Affordability-Index note's AGI proxy (sum of income
#                 items, gross Social Security, no adjustments)
#   agi_proxy     src/agi_proxy.R
# Metrics, filers with weight > 0, weighted: share within $100 or 1% of
# Tax-Simulator's AGI; share in the same Table 1.6 AGI class; each computed
# piece against Tax-Simulator's; returns with wages by class under each
# definition against Table 1.4 (resources/filer_refit_targets.csv, t14_wages).
#
# Writes results/agi_proxy_check.csv, results/agi_proxy_components.csv and
# results/agi_proxy_classes.csv. Login-node safe (~1 GB), a minute or two.
#   Rscript research/state_weights/filer_refit/02_agi_proxy_check.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(yaml) })
source('src/nonfilers/state_weights.R')
source('src/agi_proxy.R')

YEARS      <- c(2017L, 2019L, 2023L)
TS_RUN     <- 'phase0_s33'
TD_VINTAGE <- '2026092220_s33_scratch'
RES        <- 'research/state_weights/filer_refit/results'
# Agreement band for "matches": within this many dollars, or this share
MATCH_DOLLARS <- 100
MATCH_SHARE   <- 0.01

ts_dir <- file.path(model_data_root(), 'Tax-Simulator/v1', TS_RUN)
deps <- fread(file.path(ts_dir, 'dependencies.csv'))
stopifnot(deps[interface == 'Tax-Data', vintage] == TD_VINTAGE)

TD_COLS <- c('id', 'weight', 'filer', 'filing_status', 'dep_status',
             'wages', 'wages1', 'wages2', 'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
             'state_ref', 'alimony', 'divorce_year', 'sole_prop', 'sole_prop1', 'sole_prop2',
             'farm', 'farm1', 'farm2', 'part_se1', 'part_se2', 'kg_st', 'kg_lt', 'other_gains',
             'txbl_ira_dist', 'gross_pens_dist', 'txbl_pens_dist', 'rent', 'rent_loss',
             'estate', 'estate_loss', 'part_active', 'part_active_loss', 'part_passive',
             'part_passive_loss', 'part_179', 'scorp_active', 'scorp_active_loss',
             'scorp_passive', 'scorp_passive_loss', 'scorp_179', 'ui', 'gross_ss', 'other_inc',
             'ed_exp', 'hsa_contr', 'trad_contr_ira', 'sl_int_ded', 'keogh_contr', 'se_health',
             'early_penalty', 'alimony_exp', 'tuition_ded', 'dpad', 'other_above_ded', 'char_cash')
TS_COLS <- c('id', 'weight', 'filer', 'agi', 'txbl_ss', 'txbl_kg', 'sch_e', 'above_ded')

targets <- fread('resources/filer_refit_targets.csv')
class_breaks <- sort(unique(targets[source == 't14_wages', agi_lo]))

checks <- list(); components <- list(); classes <- list()
for (y in YEARS) {
  td <- fread(file.path(model_data_root(), 'Tax-Data/v1', TD_VINTAGE, 'baseline',
                        sprintf('tax_units_%d.csv', y)), select = TD_COLS)
  ts <- fread(file.path(ts_dir, 'baseline/static/detail', sprintf('%d.csv', y)), select = TS_COLS)
  td <- td[filer == 1 & weight > 0]
  ts <- ts[filer == 1 & weight > 0]
  setnames(ts, c('weight', 'filer'), c('ts_weight', 'ts_filer'))
  d <- merge(td, ts, by = 'id')
  # the two files must hold the same filers at the same weights
  stopifnot(nrow(d) == nrow(td), nrow(d) == nrow(ts), max(abs(d$weight - d$ts_weight)) < 1e-6)
  n_na <- d[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = setdiff(TD_COLS, 'divorce_year')]
  if (any(unlist(n_na) > 0)) stop(y, ': missing values in ', paste(names(n_na)[unlist(n_na) > 0], collapse = ', '))

  d <- as.data.table(agi_proxy(as_tibble(d), y))
  d[, total_income := wages + sole_prop + farm + txbl_int + div_ord + div_pref +
                      (rent - rent_loss) + gross_ss + gross_pens_dist + txbl_ira_dist +
                      other_inc + kg_st + kg_lt]

  cls <- function(x) findInterval(x, class_breaks)   # class index: the last lower bound <= x
  d[, class_ts := cls(agi)]
  for (tier in c('total_income', 'agi_proxy')) {
    d[, `:=`(v = get(tier), class_v = cls(get(tier)))]
    d[, near := abs(v - agi) <= pmax(MATCH_DOLLARS, MATCH_SHARE * abs(agi))]
    checks[[length(checks) + 1]] <- data.table(
      year = y, tier, n = nrow(d),
      share_matching = d[, sum(weight * near) / sum(weight)],
      share_same_class = d[, sum(weight * (class_v == class_ts)) / sum(weight)],
      share_same_class_under_25k = d[agi < 25e3, sum(weight * (class_v == class_ts)) / sum(weight)],
      mean_abs_diff = d[, sum(weight * abs(v - agi)) / sum(weight)])
    classes[[length(classes) + 1]] <- d[wages > 0, .(returns = sum(weight)),
                                        by = .(agi_lo = class_breaks[class_v])][, `:=`(year = y, definition = tier)]
  }
  classes[[length(classes) + 1]] <- d[wages > 0, .(returns = sum(weight)),
                                      by = .(agi_lo = class_breaks[class_ts])][, `:=`(year = y, definition = 'tax_simulator')]
  for (p in list(c('txbl_ss_proxy', 'txbl_ss'), c('txbl_kg_proxy', 'txbl_kg'),
                 c('sch_e_proxy', 'sch_e'), c('above_ded_proxy', 'above_ded'))) {
    a <- d[[p[1]]]; b <- d[[p[2]]]
    components[[length(components) + 1]] <- data.table(
      year = y, piece = p[2],
      share_exact = sum(d$weight * (abs(a - b) <= 1)) / sum(d$weight),
      share_nonzero_diff_gt_100 = sum(d$weight * (abs(a - b) > 100)) / sum(d$weight),
      total_proxy_bn = sum(d$weight * a) / 1e9, total_ts_bn = sum(d$weight * b) / 1e9)
  }
}
checks <- rbindlist(checks); components <- rbindlist(components)
classes <- dcast(rbindlist(classes), year + agi_lo ~ definition, value.var = 'returns', fill = 0)
classes <- merge(classes, targets[source == 't14_wages', .(year, agi_lo, soi_t14 = target_value)],
                 by = c('year', 'agi_lo'), all.x = TRUE)

dir.create(RES, showWarnings = FALSE, recursive = TRUE)
fwrite(checks, file.path(RES, 'agi_proxy_check.csv'))
fwrite(components, file.path(RES, 'agi_proxy_components.csv'))
fwrite(classes, file.path(RES, 'agi_proxy_classes.csv'))

print(checks, digits = 3)
print(components, digits = 3)
print(classes[agi_lo < 1e5, .(year, agi_lo, soi_t14, tax_simulator, agi_proxy, total_income)], digits = 3)
