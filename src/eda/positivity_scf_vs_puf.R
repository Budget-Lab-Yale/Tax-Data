#--------------------------------------
# positivity_scf_vs_puf.R
#
# Diagnostic: positivity rates (share of
# tax units with x > 0) for each of the
# 7 SCF↔PUF composition candidates, plus
# income itself. Two plots:
#
#   (1) Bar chart: weighted fraction
#       positive, SCF vs PUF side-by-side,
#       one pair per variable.
#   (2) Panel plot: positivity rate by
#       within-dataset income percentile,
#       one panel per variable, SCF and
#       PUF as overlaid lines.
#
# Reads only the cached inputs
#   resources/cache/scf_tax_units.rds
#   resources/cache/consumption_analysis.rds
# so it runs without touching the
# pipeline.
#
# Composition expressions mirror
# src/eda/test_wealth_X_ablation.R.
#--------------------------------------

library(tidyverse)
library(Hmisc)
source('./src/imputations/helpers.R')  # compute_percentile

plot_dir = 'plots/wealth_ablation'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

#---------------------------------------------------------------------------
# Load caches
#---------------------------------------------------------------------------

scf = read_rds('resources/cache/scf_tax_units.rds')
puf_raw = read_rds('resources/cache/consumption_analysis.rds')

#---------------------------------------------------------------------------
# Derive composition vars on each side with matched expressions.
# PUF:
#   business = sole_prop + farm + partnership(net) + S-corp(net)
#   rent     = rent - rent_loss + estate - estate_loss
#   int_div  = txbl_int + exempt_int + div_ord + div_pref
#   capital_gains = kg_lt + kg_st
#   ss_pens  = gross_ss + gross_pens_dist
#   ui_other = ui
# SCF composition is already bundled in scf_tax_units columns.
#
# Income expression: same as wealth.R / consumption.R. Re-derive explicitly
# so we have a consistent within-dataset rank on each side.
#---------------------------------------------------------------------------

puf = puf_raw %>%
  transmute(
    weight,
    wages         = wages,
    business      = sole_prop + farm +
                    scorp_active  - scorp_active_loss  - scorp_179 +
                    scorp_passive - scorp_passive_loss +
                    part_active   - part_active_loss   - part_179 +
                    part_passive  - part_passive_loss,
    int_div       = txbl_int + exempt_int + div_ord + div_pref,
    capital_gains = kg_lt + kg_st,
    rent          = rent - rent_loss + estate - estate_loss,
    ss_pens       = gross_ss + gross_pens_dist,
    ui_other      = ui,
    income        = wages + business + int_div + capital_gains +
                    rent + ss_pens + ui_other,
    source = 'PUF'
  )

scf = scf %>%
  transmute(
    weight,
    wages         = wages_scf,
    business      = business_scf,
    int_div       = int_div_scf,
    capital_gains = capital_gains_scf,
    rent          = rent_scf,
    ss_pens       = ss_pens_scf,
    ui_other      = ui_other_scf,
    income        = wages + business + int_div + capital_gains +
                    rent + ss_pens + ui_other,
    source = 'SCF'
  )

comp_vars = c('income','wages','business','int_div','capital_gains',
              'rent','ss_pens','ui_other')

#---------------------------------------------------------------------------
# (1) Overall positivity rate: weighted share with x > 0, per var × source.
#---------------------------------------------------------------------------

pos_rate = function(d) {
  map_dfr(comp_vars, function(v) {
    x = d[[v]]; w = d$weight
    tibble(variable = v,
           share_pos  = sum(w[x >  0]) / sum(w),
           share_zero = sum(w[x == 0]) / sum(w),
           share_neg  = sum(w[x <  0]) / sum(w))
  })
}

tb_scf = pos_rate(scf) %>% mutate(source = 'SCF')
tb_puf = pos_rate(puf) %>% mutate(source = 'PUF')
bar_df = bind_rows(tb_scf, tb_puf) %>%
  mutate(variable = factor(variable, levels = comp_vars),
         source   = factor(source,   levels = c('PUF', 'SCF')))

write_csv(bar_df, file.path(plot_dir, 'positivity_bar.csv'))

cat('--- Weighted positivity rates (share with x > 0) ---\n')
print(bar_df %>%
        select(variable, source, share_pos) %>%
        pivot_wider(names_from = source, values_from = share_pos) %>%
        mutate(gap_pp = 100 * (PUF - SCF)))

p_bar = bar_df %>%
  ggplot(aes(variable, share_pos, fill = source)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf('%.0f%%', 100 * share_pos)),
            position = position_dodge(width = 0.75),
            vjust = -0.3, size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.02),
                     expand = expansion(c(0, 0.05))) +
  scale_fill_manual(values = c(PUF = '#377eb8', SCF = '#e41a1c')) +
  labs(title = 'Weighted positivity rate: SCF vs PUF',
       subtitle = 'Share of tax units with value > 0, by variable',
       x = NULL, y = 'Share positive') +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(file.path(plot_dir, '05_positivity_bar.png'),
       p_bar, width = 10, height = 5, dpi = 140)

#---------------------------------------------------------------------------
# (2) Positivity by income percentile. Within-dataset rank on each side.
#---------------------------------------------------------------------------

scf$pctile = compute_percentile(scf$income, scf$weight)
puf$pctile = compute_percentile(puf$income, puf$weight)

# Drop pctile == 0 (non-positive income) from the line plot; it compresses
# into a single point at 0 which is uninformative compared to the 1..100
# curve over positive-income rows. Report how many rows that excludes.
cat(sprintf('\nrows with pctile == 0 (non-pos income): PUF %d (%.1f%%), SCF %d (%.1f%%)\n',
            sum(puf$pctile == 0), 100 * sum(puf$pctile == 0) / nrow(puf),
            sum(scf$pctile == 0), 100 * sum(scf$pctile == 0) / nrow(scf)))

pos_by_pctile = function(d) {
  d %>%
    filter(pctile >= 1) %>%
    pivot_longer(all_of(comp_vars), names_to = 'variable', values_to = 'x') %>%
    group_by(variable, pctile) %>%
    summarise(share_pos = sum(weight[x > 0]) / sum(weight),
              n_rows   = n(),
              .groups  = 'drop')
}

line_df = bind_rows(
  pos_by_pctile(scf) %>% mutate(source = 'SCF'),
  pos_by_pctile(puf) %>% mutate(source = 'PUF')
) %>%
  mutate(variable = factor(variable, levels = comp_vars),
         source   = factor(source,   levels = c('PUF', 'SCF')))

write_csv(line_df, file.path(plot_dir, 'positivity_by_pctile.csv'))

p_line = line_df %>%
  ggplot(aes(pctile, share_pos, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c(PUF = '#377eb8', SCF = '#e41a1c')) +
  facet_wrap(~ variable, ncol = 2, scales = 'free_y') +
  labs(title = 'Positivity rate by within-dataset income percentile',
       subtitle = 'Share of tax units with variable > 0 at each percentile',
       x = 'Income percentile (within-dataset)',
       y = 'Share positive') +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = 'bold'))

ggsave(file.path(plot_dir, '06_positivity_by_pctile.png'),
       p_line, width = 11, height = 11, dpi = 140)

cat(sprintf('\nPlots written:\n  %s\n  %s\n',
            file.path(plot_dir, '05_positivity_bar.png'),
            file.path(plot_dir, '06_positivity_by_pctile.png')))
cat('Done.\n')
