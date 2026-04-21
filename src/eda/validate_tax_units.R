#--------------------------------------
# validate_tax_units.R
#
# Post-build validation for Stage 1.
# Runs against the cached scf_tax_units
# and hands the user a numbers report
# plus a few diagnostic plots. Checks
# the plan's validation section end-to
# -end:
#
#   1. Per-household field preservation
#      (allocation shares sum to 1)
#   2. Tax-unit counts and split rate
#   3. FILESTAT distribution vs IRS
#   4. Weighted aggregates vs SCF
#      Bulletin published totals
#   5. Age, n_dep, weight marginals
#
# Output: plots/stage1_validation.txt
# and plots/stage1/*.png
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/configure.R')

report_path = 'plots/stage1_validation.txt'
plot_dir    = 'plots/stage1'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

sink(report_path, split = TRUE)

cat('Stage 1 validation report\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 60), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# Load artifacts
#---------------------------------------------------------------------------

stu_path = 'resources/cache/scf_tax_units.rds'
if (!file.exists(stu_path)) {
  sink()
  stop('scf_tax_units cache missing at ', stu_path,
       ' — run src/eda/test_stage1.R or src/imputations/stage1_scf_tax_units.R first')
}

stu  = read_rds(stu_path)
scfp = fread(interface_paths$SCF %>% file.path('SCFP2022.csv')) %>% tibble()

cat('Tax-unit rows: ', nrow(stu),    '\n')
cat('Source HHs:    ', n_distinct(stu$scf_hh_id), '\n')
cat('SCFP rows:     ', nrow(scfp),   '\n\n')

#---------------------------------------------------------------------------
# 1. Per-household preservation
#
# For every wealth category, sum across tax units per (HH, implicate) and
# verify the total matches the SCFP household row to floating-point.
#---------------------------------------------------------------------------

cat('1. Per-household preservation (max absolute $ diff)\n')
cat(paste0(rep('-', 60), collapse = ''), '\n')

hh_tu = stu %>%
  group_by(scf_hh_id, implicate) %>%
  summarise(across(c(cash, equities, bonds, retirement, life_ins, annuities,
                     trusts, other_fin, pass_throughs, primary_home,
                     other_home, re_fund, other_nonfin,
                     primary_mortgage, other_mortgage, credit_lines,
                     credit_cards, installment_debt, other_debt,
                     kg_primary_home, kg_other_re, kg_pass_throughs, kg_other),
                   sum),
            .groups = 'drop')

scfp_side = scfp %>%
  mutate(implicate = as.integer(Y1 - 10L * YY1)) %>%
  transmute(
    scf_hh_id = YY1, implicate,
    scfp_cash             = LIQ + CDS,
    scfp_equities         = STOCKS + STMUTF + COMUTF,
    scfp_bonds            = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    scfp_retirement       = IRAKH + THRIFT + FUTPEN + CURRPEN,
    scfp_life_ins         = CASHLI,
    scfp_annuities        = ANNUIT,
    scfp_trusts           = TRUSTS,
    scfp_other_fin        = OTHFIN + OMUTF,
    scfp_pass_throughs    = BUS,
    scfp_primary_home     = HOUSES,
    scfp_other_home       = ORESRE,
    scfp_re_fund          = NNRESRE,
    scfp_other_nonfin     = VEHIC + OTHNFIN,
    scfp_primary_mortgage = MRTHEL,
    scfp_other_mortgage   = RESDBT,
    scfp_credit_lines     = OTHLOC,
    scfp_credit_cards     = CCBAL,
    scfp_installment_debt = INSTALL,
    scfp_other_debt       = ODEBT,
    scfp_kg_primary_home  = KGHOUSE,
    scfp_kg_other_re      = KGORE,
    scfp_kg_pass_throughs = KGBUS,
    scfp_kg_other         = KGSTMF
  )

cmp = hh_tu %>% inner_join(scfp_side, by = c('scf_hh_id', 'implicate'))

preservation_check = tribble(
  ~field,              ~diff,
  'cash',              max(abs(cmp$cash             - cmp$scfp_cash            )),
  'equities',          max(abs(cmp$equities         - cmp$scfp_equities        )),
  'bonds',             max(abs(cmp$bonds            - cmp$scfp_bonds           )),
  'retirement',        max(abs(cmp$retirement       - cmp$scfp_retirement      )),
  'life_ins',          max(abs(cmp$life_ins         - cmp$scfp_life_ins        )),
  'annuities',         max(abs(cmp$annuities        - cmp$scfp_annuities       )),
  'trusts',            max(abs(cmp$trusts           - cmp$scfp_trusts          )),
  'other_fin',         max(abs(cmp$other_fin        - cmp$scfp_other_fin       )),
  'pass_throughs',     max(abs(cmp$pass_throughs    - cmp$scfp_pass_throughs   )),
  'primary_home',      max(abs(cmp$primary_home     - cmp$scfp_primary_home    )),
  'other_home',        max(abs(cmp$other_home       - cmp$scfp_other_home      )),
  're_fund',           max(abs(cmp$re_fund          - cmp$scfp_re_fund         )),
  'other_nonfin',      max(abs(cmp$other_nonfin     - cmp$scfp_other_nonfin    )),
  'primary_mortgage',  max(abs(cmp$primary_mortgage - cmp$scfp_primary_mortgage)),
  'other_mortgage',    max(abs(cmp$other_mortgage   - cmp$scfp_other_mortgage  )),
  'credit_lines',      max(abs(cmp$credit_lines     - cmp$scfp_credit_lines    )),
  'credit_cards',      max(abs(cmp$credit_cards     - cmp$scfp_credit_cards    )),
  'installment_debt',  max(abs(cmp$installment_debt - cmp$scfp_installment_debt)),
  'other_debt',        max(abs(cmp$other_debt       - cmp$scfp_other_debt      )),
  'kg_primary_home',   max(abs(cmp$kg_primary_home  - cmp$scfp_kg_primary_home )),
  'kg_other_re',       max(abs(cmp$kg_other_re      - cmp$scfp_kg_other_re     )),
  'kg_pass_throughs',  max(abs(cmp$kg_pass_throughs - cmp$scfp_kg_pass_throughs)),
  'kg_other',          max(abs(cmp$kg_other         - cmp$scfp_kg_other        ))
)

for (i in seq_len(nrow(preservation_check))) {
  flag = if (preservation_check$diff[i] < 1) 'PASS' else 'FAIL'
  cat(sprintf('  %-20s %12.2f   [%s]\n',
              preservation_check$field[i], preservation_check$diff[i], flag))
}
cat('\n')

#---------------------------------------------------------------------------
# 2. Tax-unit counts and split rate
#---------------------------------------------------------------------------

cat('2. Tax-unit structure\n')
cat(paste0(rep('-', 60), collapse = ''), '\n')

tu_per_hh = stu %>%
  group_by(scf_hh_id, implicate) %>%
  summarise(n = n(), .groups = 'drop') %>%
  count(n, name = 'n_hhs')
cat('  Tax-unit count per household (across implicates):\n')
for (i in seq_len(nrow(tu_per_hh)))
  cat(sprintf('    %d tax units : %5d HH-implicate pairs\n',
              tu_per_hh$n[i], tu_per_hh$n_hhs[i]))

split_tbl = table(stu$taxunit_code)
cat('\n  taxunit_code counts (0=single-PEU, 1/2=split-PEU, >=3=NPEU):\n')
for (nm in names(split_tbl))
  cat(sprintf('    TU=%s : %6d\n', nm, as.integer(split_tbl[nm])))

# Split / NPEU rates PER IMPLICATE (not across all 5 — across-implicate
# classifications can differ because imputed values shift).
per_impl_rates = stu %>%
  group_by(implicate) %>%
  summarise(
    split_hhs = n_distinct(scf_hh_id[taxunit_code == 1]),
    npeu_hhs  = n_distinct(scf_hh_id[taxunit_code >= 3]),
    total_hhs = n_distinct(scf_hh_id),
    .groups   = 'drop'
  ) %>%
  mutate(split_rate = split_hhs / total_hhs,
         npeu_rate  = npeu_hhs  / total_hhs)

cat(sprintf('\n  Split-PEU rate (per implicate): %.2f%%\n',
            100 * mean(per_impl_rates$split_rate)))
cat(sprintf('  NPEU rate      (per implicate): %.2f%%\n',
            100 * mean(per_impl_rates$npeu_rate)))

#---------------------------------------------------------------------------
# 3. FILESTAT distribution
#
# Benchmark: IRS 2022 individual returns filed:
#   Single:    ~59M (~38%)
#   MFJ:       ~56M (~36%)
#   MFS:       ~2.7M (~2%)
#   HoH:       ~24M (~15%)
# SCF-side aggregates will differ because SCF covers all HHs, not just
# filers; the ratios should still be in the same ballpark.
#---------------------------------------------------------------------------

cat('\n3. FILESTAT distribution (on tax units)\n')
cat(paste0(rep('-', 60), collapse = ''), '\n')

fs_tbl = stu %>% count(filestat, name = 'n') %>%
  mutate(share = n / sum(n),
         filestat = case_when(
           filestat == 1 ~ 'Single',
           filestat == 2 ~ 'MFJ',
           filestat == 3 ~ 'MFS',
           filestat == 4 ~ 'HoH',
           TRUE          ~ 'Unknown'
         ))
for (i in seq_len(nrow(fs_tbl)))
  cat(sprintf('  %-8s : %6d (%.1f%%)\n',
              fs_tbl$filestat[i], fs_tbl$n[i], 100 * fs_tbl$share[i]))

#---------------------------------------------------------------------------
# 4. Weighted aggregates vs SCF Bulletin
#
# Published SCF Bulletin 2022 aggregates (Federal Reserve, Sept 2023):
#   Total assets   : ~$158T
#   Total debts    : ~$17T
#   Net worth      : ~$140T
# Our aggregates should match within rounding (exact HH preservation
# guarantees this once we sum the per-tax-unit cells).
#---------------------------------------------------------------------------

cat('\n4. Weighted aggregates ($ billions)\n')
cat(paste0(rep('-', 60), collapse = ''), '\n')

wsum_bn = function(col) sum(stu$weight * col) / 1e9

asset_cols = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_cols  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')

agg_assets = sum(sapply(asset_cols, function(c) wsum_bn(stu[[c]])))
agg_debts  = sum(sapply(debt_cols,  function(c) wsum_bn(stu[[c]])))
agg_nw     = agg_assets - agg_debts

cat(sprintf('  Total assets  : %8.0f B (SCF Bulletin: ~158,000 B)\n', agg_assets))
cat(sprintf('  Total debts   : %8.0f B (SCF Bulletin: ~17,000 B)\n',  agg_debts))
cat(sprintf('  Net worth     : %8.0f B (SCF Bulletin: ~140,000 B)\n', agg_nw))

cat('\n  Per-category breakdown:\n')
for (col in c(asset_cols, debt_cols)) {
  v = wsum_bn(stu[[col]])
  cat(sprintf('    %-18s : %8.1f B\n', col, v))
}

#---------------------------------------------------------------------------
# 5. Demographic marginals
#---------------------------------------------------------------------------

cat('\n5. Demographics\n')
cat(paste0(rep('-', 60), collapse = ''), '\n')

cat(sprintf('  age1 summary:  min=%.0f  med=%.0f  mean=%.1f  max=%.0f\n',
            min(stu$age1, na.rm = TRUE),
            median(stu$age1, na.rm = TRUE),
            mean(stu$age1, na.rm = TRUE),
            max(stu$age1, na.rm = TRUE)))
cat(sprintf('  married share: %.1f%%\n',
            100 * weighted.mean(stu$married, stu$weight)))
cat(sprintf('  n_dep summary: mean=%.2f  median=%.0f  max=%d\n',
            mean(stu$n_dep), median(stu$n_dep), max(stu$n_dep)))
cat(sprintf('  male1 share  : %.1f%% (NA: %d)\n',
            100 * weighted.mean(stu$male1, stu$weight, na.rm = TRUE),
            sum(is.na(stu$male1))))

sum_w = sum(stu$weight)
cat(sprintf('\n  Total weighted tax units: %.1f million\n', sum_w / 1e6))
cat(sprintf('  (IRS 2022 individual returns filed: ~160M)\n'))

#---------------------------------------------------------------------------
# 6. Plots
#---------------------------------------------------------------------------

# Age pyramid
p_age = ggplot(stu, aes(age1, weight = weight)) +
  geom_histogram(binwidth = 2, fill = 'steelblue', color = 'white') +
  labs(title = 'Stage 1: weighted age1 distribution of tax units',
       x = 'Primary age', y = 'Weighted count') +
  theme_minimal(base_size = 10)
ggsave(file.path(plot_dir, '01_age_distribution.png'), p_age,
       width = 8, height = 4, dpi = 140)

# Filestat share
p_fs = fs_tbl %>%
  mutate(filestat = factor(filestat, levels = fs_tbl$filestat)) %>%
  ggplot(aes(filestat, share, fill = filestat)) +
  geom_col() +
  geom_text(aes(label = sprintf('%.1f%%', 100 * share)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Stage 1: filing status distribution on tax units',
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = 'none')
ggsave(file.path(plot_dir, '02_filestat.png'), p_fs,
       width = 7, height = 4, dpi = 140)

# Aggregate-by-category bar (billions)
agg_tbl = tibble(
  category = c(asset_cols, debt_cols),
  bucket   = c(rep('asset', length(asset_cols)), rep('debt', length(debt_cols))),
  value_bn = sapply(c(asset_cols, debt_cols), function(c) wsum_bn(stu[[c]]))
)
p_agg = agg_tbl %>%
  mutate(category = factor(category, levels = category[order(value_bn)])) %>%
  ggplot(aes(category, value_bn, fill = bucket)) +
  geom_col() +
  scale_fill_manual(values = c('asset' = '#4daf4a', 'debt' = '#e41a1c')) +
  coord_flip() +
  labs(title = 'Stage 1: weighted aggregate per wealth category',
       x = NULL, y = '$ billions') +
  theme_minimal(base_size = 9)
ggsave(file.path(plot_dir, '03_aggregates.png'), p_agg,
       width = 9, height = 6, dpi = 140)

cat(sprintf('\nPlots saved to %s/\n', plot_dir))
cat('Report saved to ', report_path, '\n', sep = '')
sink()
