#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 06_puf_soi_gaps.R  (S40 follow-up) -- where the PUF and Pub 1304 disagree
#
# create_2017_puf.R rescales TY2017 amounts to Table 1.4 by AGI group. The gap
# it closes mixes three things: definitions (PUF line vs Table 1.4 series),
# PUF construction (subsampling, blurring, the four aggregate records that
# process_puf.R drops), and 2015 -> 2017 growth (the file is the TY2015 PUF;
# stage 0/1 move weights to 2017 counts, but no amount is aged before the
# rescaling). Two comparisons separate them:
#
#   2015  the raw TY2015 PUF on its own weights (S006 / 100) against TY2015
#         Table 1.4 (IRS-Ind aligned panel), with and without the aggregate
#         records -- same year, so a gap here is definitions or construction
#   2017  the file on stage-1 weights, before the rescaling, against the TY2017
#         Table 1.4 the pipeline uses (Compiled-SOI-Tables): summary_stats as
#         create_2017_puf.R computes it -- the gap the rescaling closed
#
# Runs Phase 1 through create_2017_puf.R with main.R's seed. configure.R is
# evaluated without its two writes (the vintage directory and its
# dependencies.csv under the shared output root).
#
# Writes results/puf_soi_gaps/gaps_{2015,2017}_{totals,by_agi}.csv.
#   sbatch ... research/state_weights/filer_refit/run_puf_soi_gaps.sbatch
#------------------------------------------------------------------------------

lapply(readLines('requirements.txt'), library, character.only = T)

RES = 'research/state_weights/filer_refit/results/puf_soi_gaps'
dir.create(RES, recursive = TRUE, showWarnings = FALSE)
AGGREGATE_RECID_MIN = 999996   # process_puf.R: RECID >= this is an aggregate record

# configure.R without the writes to the shared output root
configure_exprs = parse('./src/configure.R')
writes = map_lgl(configure_exprs, ~ any(grepl('dir.create\\(output_path|dependencies\\.csv',
                                               deparse(.x))))
stopifnot(sum(writes) == 2)
for (e in configure_exprs[!writes]) eval(e, envir = globalenv())

set.seed(76)   # main.R's seed, so the age imputation and stage-1 weights match
source('./src/process_targets.R')
source('./src/process_puf.R')
source('./src/reweight.R')
source('./src/summary.R')
source('./src/create_2017_puf.R')
# summary_stats is create_2017_puf.R's comparison before the rescaling


#------------------------------------------------------------------------------
# 2015: raw PUF vs TY2015 Table 1.4
#------------------------------------------------------------------------------

# Tax-Data name, PUF line, Table 1.4 series, part of the line (all / net
# income = positive part / net loss = magnitude of the negative part)
SERIES_MAP = tribble(
  ~variable,            ~puf,     ~series,                                                                       ~part,
  'wages',              'E00200', 'wages',                                                                       'all',
  'txbl_int',           'E00300', 'taxable interest',                                                            'all',
  'exempt_int',         'E00400', 'tax exempt interest',                                                         'all',
  'div',                'E00600', 'ordinary dividends',                                                          'all',
  'div_pref',           'E00650', 'qualified dividends',                                                         'all',
  'state_ref',          'E00700', 'state income tax refunds',                                                    'all',
  'alimony',            'E00800', 'alimony received',                                                            'all',
  'sole_prop.income',   'E00900', 'business or profession > net income',                                         'pos',
  'sole_prop.loss',     'E00900', 'business or profession > net loss',                                           'neg',
  'txbl_kg.income',     'E01000', 'sales of capital assets reported on form 1040, schedule d > taxable net gain', 'pos',
  'txbl_kg.loss',       'E01000', 'sales of capital assets reported on form 1040, schedule d > taxable net loss', 'neg',
  'kg_dist',            'E01100', 'capital gain distributions reported on form 1040',                            'all',
  'other_gains.income', 'E01200', 'sales of property other than capital assets > net gain',                      'pos',
  'other_gains.loss',   'E01200', 'sales of property other than capital assets > net loss',                      'neg',
  'txbl_ira_dist',      'E01400', 'taxable individual retirement arrangement (ira) distributions',               'all',
  'gross_pens_dist',    'E01500', 'pensions and annuities > total',                                              'all',
  'txbl_pens_dist',     'E01700', 'pensions and annuities > taxable',                                            'all',
  'part_scorp.income',  'E26270', 'partnership and s corporation > net income',                                  'pos',
  'part_scorp.loss',    'E26270', 'partnership and s corporation > net loss',                                    'neg',
  'farm.income',        'E02100', 'farm > net income',                                                           'pos',
  'farm.loss',          'E02100', 'farm > net loss',                                                             'neg',
  'ui',                 'E02300', 'unemployment compensation',                                                   'all',
  'gross_ss',           'E02400', 'social security benefits > total',                                            'all',
  'txbl_ss',            'E02500', 'social security benefits > taxable',                                          'all'
)

source('src/nonfilers/sw_common.R')   # raw_data_root()
source('src/nonfilers/sw_soi.R')      # irs_ind_aligned_path()
# Amounts in $ thousands. The panel total is the row with agi_lo = NA. TY2015
# publishes $200-500k as one class: the panel's $200-250k and $250-500k rows
# are empty that year, so only the valued rows are classes.
soi_2015_panel = fread(irs_ind_aligned_path('panel_income_sources.csv'), na.strings = '') %>%
  as_tibble() %>%
  filter(tax_year == 2015, panel == 'all returns', series %in% SERIES_MAP$series,
         measure %in% c('amount', 'returns')) %>%
  select(series, agi_lo, agi_hi, measure, value)
soi_2015 = soi_2015_panel %>% filter(!is.na(agi_lo), !is.na(value))
soi_2015_total = soi_2015_panel %>% filter(is.na(agi_lo)) %>% select(series, measure, total = value)
stopifnot(setequal(unique(soi_2015$series), SERIES_MAP$series),
          !anyDuplicated(soi_2015 %>% select(series, agi_lo, measure)),
          nrow(soi_2015_total) == 2 * nrow(SERIES_MAP), !any(is.na(soi_2015_total$total)))
# the classes partition the table: they add to its total
class_sums = soi_2015 %>% group_by(series, measure) %>% summarise(s = sum(value), .groups = 'drop') %>%
  inner_join(soi_2015_total, by = c('series', 'measure'))
stopifnot(nrow(class_sums) == nrow(soi_2015_total),
          all(abs(class_sums$s / class_sums$total - 1) < 1e-3 | class_sums$total == 0))
stopifnot(n_distinct(count(soi_2015, series, measure)$n) == 1)   # one class set for all
agi_breaks = sort(unique(c(soi_2015$agi_lo, soi_2015$agi_hi)))

puf_line_part = function(x, part) {
  case_when(part == 'all' ~ x, part == 'pos' ~ pmax(x, 0), part == 'neg' ~ -pmin(x, 0))
}

puf_2015_by_agi = function(records, label) {
  stopifnot(!any(is.na(records$E00100)), !any(is.na(records$S006)))
  pmap(SERIES_MAP, function(variable, puf, series, part) {
    x = puf_line_part(records[[puf]], part)
    # The aggregate records leave some lines blank (alimony is removed from
    # high-rate strata): nothing reported there. Any other NA is an error.
    agg_na = is.na(x) & records$RECID >= AGGREGATE_RECID_MIN
    if (any(agg_na)) message(puf, ': ', sum(agg_na), ' aggregate record(s) blank, counted as 0')
    x[agg_na] = 0
    stopifnot(!any(is.na(x)))
    tibble(agi_lo = agi_breaks[findInterval(records$E00100, agi_breaks)],
           x = x, w = records$S006 / 100) %>%
      group_by(agi_lo) %>%
      summarise(amount = sum(x * w) / 1e3, returns = sum((x != 0) * w), .groups = 'drop') %>%
      mutate(variable = variable, series = series)
  }) %>%
    bind_rows() %>%
    pivot_longer(c(amount, returns), names_to = 'measure', values_to = label)
}

gaps_2015_by_agi = soi_2015 %>%
  rename(soi = value) %>%
  left_join(puf_2015_by_agi(raw_puf %>% filter(RECID < AGGREGATE_RECID_MIN), 'puf'),
            by = c('series', 'agi_lo', 'measure')) %>%
  left_join(puf_2015_by_agi(raw_puf, 'puf_with_agg') %>% select(-variable),
            by = c('series', 'agi_lo', 'measure')) %>%
  mutate(across(c(puf, puf_with_agg), ~ coalesce(., 0)))   # a class holding no PUF record has 0
gaps_2015 = gaps_2015_by_agi %>%
  group_by(variable, series, measure) %>%
  summarise(soi = sum(soi), puf = sum(puf), puf_with_agg = sum(puf_with_agg),
            mean_abs_class_err = mean(abs(puf / soi - 1)[soi > 0]), .groups = 'drop') %>%
  mutate(ratio = puf / soi, ratio_with_agg = puf_with_agg / soi)

returns_2015 = tibble(
  soi_t16      = tables$table_1_6 %>% filter(year == 2015) %>% pull(count) %>% sum(),
  puf          = sum(raw_puf$S006[raw_puf$RECID < AGGREGATE_RECID_MIN]) / 100,
  puf_with_agg = sum(raw_puf$S006) / 100,
  agg_records  = sum(raw_puf$RECID >= AGGREGATE_RECID_MIN))


#------------------------------------------------------------------------------
# 2017: stage-1 weights, before the rescaling
#------------------------------------------------------------------------------

gaps_2017_by_agi = summary_stats$by_agi
gaps_2017 = summary_stats$totals %>% mutate(ratio = puf / actual)


fwrite(gaps_2015,        file.path(RES, 'gaps_2015_totals.csv'))
fwrite(gaps_2015_by_agi, file.path(RES, 'gaps_2015_by_agi.csv'))
fwrite(returns_2015,     file.path(RES, 'returns_2015.csv'))
fwrite(gaps_2017,        file.path(RES, 'gaps_2017_totals.csv'))
fwrite(gaps_2017_by_agi, file.path(RES, 'gaps_2017_by_agi.csv'))

options(width = 200)
print(returns_2015)
print(gaps_2015 %>% filter(measure == 'amount') %>% select(variable, soi, ratio, ratio_with_agg, mean_abs_class_err), n = Inf)
print(gaps_2015 %>% filter(measure == 'returns') %>% select(variable, soi, ratio, ratio_with_agg), n = Inf)
print(gaps_2017 %>% arrange(series, variable), n = Inf)
cat('PUF_SOI_GAPS_SUCCESS\n')
