#--------------------------------------
# wealth_extensive_margin.R
#
# Diagnostic: extensive-margin positivity
# rates for each of the 23 wealth Y
# categories, SCF vs PUF-imputed (4
# specs). Produced because per-category
# intensive-margin rescaling (Stage 3
# Phase 3a) cannot fix extensive-margin
# mismatches — if PUF says 30% of
# decile 1 owns equities and SCF says
# 5%, a scale factor just shrinks the
# $ amount without changing the 30%.
#
# Outputs (plots/wealth_ablation/):
#   - 13_positivity_agg_bar.png —
#       aggregate positivity, 23 vars ×
#       sources
#   - 14_positivity_by_decile.png —
#       panel grid, positivity by income
#       decile for each of the 23 Y
#       categories, 4 PUF specs + SCF
#   - positivity_agg.csv
#   - positivity_by_decile.csv
#
# Inputs:
#   resources/cache/wealth_ablation.rds
#     (postproc output; has puf_by_spec)
#   resources/cache/scf_tax_units.rds
#     (SCF truth)
#--------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(Hmisc)
})
source('./src/imputations/helpers.R')

plot_dir = 'plots/wealth_ablation'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

pp = read_rds('resources/cache/wealth_ablation.rds')
scf_raw = read_rds('resources/cache/scf_tax_units.rds')

wealth_asset_vars = c(
  'cash', 'equities', 'bonds', 'retirement', 'life_ins', 'annuities',
  'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
  're_fund', 'other_nonfin'
)
wealth_debt_vars = c(
  'primary_mortgage', 'other_mortgage', 'credit_lines',
  'credit_cards', 'installment_debt', 'other_debt'
)
wealth_kg_vars = c(
  'kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other'
)
wealth_y_vars = c(wealth_asset_vars, wealth_debt_vars, wealth_kg_vars)

# SCF-native frame
scf_native = scf_raw %>%
  mutate(
    total_assets = rowSums(across(all_of(wealth_asset_vars))),
    total_debts  = rowSums(across(all_of(wealth_debt_vars))),
    net_worth    = total_assets - total_debts,
    inc_dec      = {
      pos = income > 0
      brks = sort(wtd.quantile(income[pos], weight[pos], probs = seq(0, 1, 0.1)))
      bin = findInterval(income, brks, all.inside = TRUE)
      ifelse(pos, bin, 0L)
    }
  )

# Source dictionary
spec_names = pp$spec_names
sources = c(
  setNames(pp$puf_by_spec[spec_names], spec_names),
  list(`SCF (truth)` = scf_native)
)

# Add inc_dec to each PUF frame (SCF already has it)
for (nm in spec_names) {
  d = sources[[nm]]
  pos = d$income > 0
  brks = sort(wtd.quantile(d$income[pos], d$weight[pos], probs = seq(0, 1, 0.1)))
  d$inc_dec = ifelse(pos, findInterval(d$income, brks, all.inside = TRUE), 0L)
  sources[[nm]] = d
}

#---------------------------------------------------------------------------
# (1) Aggregate positivity rates — weighted share with var > 0
#---------------------------------------------------------------------------

pos_agg = map_dfr(names(sources), function(s) {
  d = sources[[s]]
  map_dfr(wealth_y_vars, function(v) {
    tibble(source = s, variable = v,
           share_pos = sum(d$weight[d[[v]] > 0]) / sum(d$weight))
  })
}) %>%
  mutate(
    variable = factor(variable, levels = wealth_y_vars),
    bucket   = case_when(variable %in% wealth_asset_vars ~ 'assets',
                         variable %in% wealth_debt_vars  ~ 'debts',
                         variable %in% wealth_kg_vars    ~ 'kg'),
    source   = factor(source, levels = c(spec_names, 'SCF (truth)'))
  )

write_csv(pos_agg, file.path(plot_dir, 'positivity_agg.csv'))

cat('\n==========================================================\n')
cat('  Aggregate positivity rate: weighted share with Y > 0\n')
cat('==========================================================\n\n')
pos_wide = pos_agg %>%
  select(variable, bucket, source, share_pos) %>%
  pivot_wider(names_from = source, values_from = share_pos) %>%
  mutate(
    gap_large_vs_scf = `large` - `SCF (truth)`,
    gap_medium_vs_scf = `medium` - `SCF (truth)`
  )
# Print with percentages
print(pos_wide %>%
        mutate(across(-c(variable, bucket), ~ sprintf('%.1f%%', 100*.x))),
      n = length(wealth_y_vars))

#---------------------------------------------------------------------------
# (2) Positivity by income decile, per category, per source
#---------------------------------------------------------------------------

pos_by_dec = map_dfr(names(sources), function(s) {
  d = sources[[s]]
  map_dfr(wealth_y_vars, function(v) {
    d %>%
      filter(inc_dec >= 1) %>%
      group_by(inc_dec) %>%
      summarise(share_pos = sum(weight[.data[[v]] > 0]) / sum(weight),
                .groups   = 'drop') %>%
      mutate(source = s, variable = v)
  })
}) %>%
  mutate(variable = factor(variable, levels = wealth_y_vars),
         source   = factor(source, levels = c(spec_names, 'SCF (truth)')))

write_csv(pos_by_dec, file.path(plot_dir, 'positivity_by_decile.csv'))

#---------------------------------------------------------------------------
# Plots
#---------------------------------------------------------------------------

source_pal = c(
  small       = '#4daf4a',
  medium      = '#377eb8',
  large       = '#e41a1c',
  `SCF (truth)` = 'black'
)
source_lt  = c(
  small       = 'solid',
  medium      = 'solid',
  large       = 'solid',
  `SCF (truth)` = 'dashed'
)

p_agg = pos_agg %>%
  ggplot(aes(variable, 100 * share_pos, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = source_pal) +
  facet_wrap(~ bucket, scales = 'free_x', ncol = 1) +
  labs(title = 'Extensive-margin positivity, aggregate: share of tax units with Y > 0',
       subtitle = 'Rescaling (Stage 3) operates on dollar amounts only. This is the gap it cannot fix.',
       x = NULL, y = 'Share > 0 (%)') +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = 'bold'))

ggsave(file.path(plot_dir, '13_positivity_agg_bar.png'),
       p_agg, width = 14, height = 11, dpi = 140)

p_dec = pos_by_dec %>%
  ggplot(aes(inc_dec, 100 * share_pos, color = source, linetype = source)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = source_pal) +
  scale_linetype_manual(values = source_lt) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  facet_wrap(~ variable, ncol = 5, scales = 'free_y') +
  labs(title = 'Extensive-margin positivity by income decile, per wealth category',
       subtitle = 'SCF (dashed black) = truth. PUF specs should overlap the dashed line.',
       x = 'Income decile', y = 'Share > 0 (%)') +
  theme_minimal(base_size = 8) +
  theme(strip.text = element_text(face = 'bold'),
        legend.position = 'bottom')

ggsave(file.path(plot_dir, '14_positivity_by_decile.png'),
       p_dec, width = 15, height = 11, dpi = 140)

#---------------------------------------------------------------------------
# Compact console summary: categories with worst positivity gap vs SCF
#---------------------------------------------------------------------------

cat('\n\n==========================================================\n')
cat('  Largest positivity gaps, |PUF - SCF| in pp (large spec)\n')
cat('==========================================================\n\n')
gaps = pos_agg %>%
  pivot_wider(names_from = source, values_from = share_pos) %>%
  mutate(gap_large       = 100 * (large       - `SCF (truth)`),
         gap_medium      = 100 * (medium      - `SCF (truth)`),
         gap_small       = 100 * (small       - `SCF (truth)`)) %>%
  select(variable, bucket, starts_with('gap_'))

# Show worst 5 per spec
for (col in c('gap_large', 'gap_medium', 'gap_small')) {
  cat(sprintf('\n-- Top 5 by |%s| --\n', col))
  print(gaps %>% arrange(desc(abs(.data[[col]]))) %>%
          select(variable, bucket, all_of(col)) %>%
          mutate(!!col := sprintf('%+.1f pp', .data[[col]])) %>%
          slice_head(n = 5))
}

cat(sprintf('\nPlots written:\n  %s\n  %s\n',
            file.path(plot_dir, '13_positivity_agg_bar.png'),
            file.path(plot_dir, '14_positivity_by_decile.png')))
cat('Done.\n')
