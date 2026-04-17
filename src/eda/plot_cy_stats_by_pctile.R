#--------------------------------------
# plot_cy_stats_by_pctile.R
#
# Same structure as plot_stats_by_pctile.R, but for consumption-to-income
# ratios: total C/Y and category_k/Y, by own-dataset income percentile.
#--------------------------------------

lapply(c('tidyverse', 'ggplot2', 'Hmisc', 'data.table'),
       library, character.only = TRUE)

pce_cats = c('c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
             'c_food_off_premises', 'c_gasoline', 'c_housing_utilities',
             'c_other_services_health')

plot_dir  = 'plots'
BIN_WIDTH = 5

cex = read_rds('resources/cache/cex_training_cached.rds')$cex
puf = read_rds('resources/cache/consumption_analysis.rds')

cex_h = cex %>%
  filter(has_income == 1, income > 0, total_consumption > 0,
         pctile_income >= 1) %>%
  mutate(w = WT_ANNUAL, pct = pctile_income, cy = total_consumption / income,
         across(all_of(pce_cats), ~ .x / income, .names = '{.col}_cy')) %>%
  select(w, pct, cy, ends_with('_cy'))

puf_h = puf %>%
  filter(income > 0, C > 0) %>%
  mutate(w = weight, pct = pctile, cy = C / income,
         across(all_of(pce_cats), ~ .x / income, .names = '{.col}_cy')) %>%
  select(w, pct, cy, ends_with('_cy'))

bin_pct = function(df) df %>%
  mutate(pct_bin = BIN_WIDTH * ((pct - 1) %/% BIN_WIDTH) + BIN_WIDTH / 2 + 0.5)
cex_h = bin_pct(cex_h); puf_h = bin_pct(puf_h)

# Reuse seed 42 → same random category as plot 21 (clothing)
set.seed(42)
random_cat = sample(pce_cats, 1)
cat(sprintf('Random category: %s\n', random_cat))

stat_levels = c('p10', 'p25', 'p50', 'mean', 'p75', 'p90', 'p99')

stats_per_bin = function(df, col, src) {
  df %>% group_by(pct_bin) %>%
    summarise(
      p10  = Hmisc::wtd.quantile(.data[[col]], w, 0.10, na.rm = TRUE),
      p25  = Hmisc::wtd.quantile(.data[[col]], w, 0.25, na.rm = TRUE),
      p50  = Hmisc::wtd.quantile(.data[[col]], w, 0.50, na.rm = TRUE),
      mean = weighted.mean  (.data[[col]], w,       na.rm = TRUE),
      p75  = Hmisc::wtd.quantile(.data[[col]], w, 0.75, na.rm = TRUE),
      p90  = Hmisc::wtd.quantile(.data[[col]], w, 0.90, na.rm = TRUE),
      p99  = Hmisc::wtd.quantile(.data[[col]], w, 0.99, na.rm = TRUE),
      n    = n(),
      .groups = 'drop'
    ) %>% mutate(source = src)
}

make_panel = function(col, title_lbl, outfile) {
  d = bind_rows(stats_per_bin(cex_h, col, 'CEX'),
                stats_per_bin(puf_h, col, 'PUF')) %>%
    pivot_longer(all_of(stat_levels), names_to = 'stat', values_to = 'val') %>%
    mutate(stat = factor(stat, levels = stat_levels))

  p = ggplot(d, aes(pct_bin, val, color = source)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.1) +
    facet_wrap(~ stat, ncol = 4, scales = 'free_y') +
    scale_color_manual(values = c(CEX = '#2c7bb6', PUF = '#d7191c'),
                       name = NULL) +
    labs(title    = sprintf('%s by own-dataset income percentile', title_lbl),
         subtitle = sprintf('%d-pt pctile bins. y is ratio of consumption to income.  CEX vs PUF overlay.',
                            BIN_WIDTH),
         x = 'Income percentile (own-dataset rank)',
         y = 'ratio') +
    theme_minimal(base_size = 10) +
    theme(legend.position  = 'top',
          panel.grid.minor = element_blank(),
          strip.text       = element_text(face = 'bold'))

  ggsave(outfile, p, width = 12, height = 7, dpi = 140)
  cat(sprintf('Saved: %s\n', outfile))
}

make_panel('cy',
           'Consumption-to-income ratio (C/Y)',
           file.path(plot_dir, '22_cy_stats_by_pctile.png'))

make_panel(paste0(random_cat, '_cy'),
           sprintf('%s / income', random_cat),
           file.path(plot_dir,
             sprintf('23_%s_cy_stats_by_pctile.png', random_cat)))
