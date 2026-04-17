#--------------------------------------
# plot_cell_quantile_sample.R
#
# For a random set of cells with >= MIN_N obs on both sides, plots the
# per-percentile quantile curve F^-1(p) of total consumption, one line
# each for CEX and PUF.
#--------------------------------------

lapply(c('tidyverse', 'ggplot2'), library, character.only = TRUE)

MIN_N    = 250
N_CELLS  = 10
SEED     = 42

d = read_rds('resources/cache/conditional_diag_stageA_pctile.rds')

eligible = d$cell_n %>% filter(n_cex >= MIN_N, n_puf >= MIN_N)
cat(sprintf('Eligible cells (n>=%d both sides): %d / %d\n',
            MIN_N, nrow(eligible), nrow(d$cell_n)))

set.seed(SEED)
sampled = eligible %>% sample_n(min(N_CELLS, nrow(eligible))) %>% arrange(cell)

cat('\nSampled cells:\n')
print(sampled, n = N_CELLS)

ql = d$quantile_long %>% filter(cell %in% sampled$cell) %>%
  left_join(eligible %>% select(cell, n_cex, n_puf), by = 'cell') %>%
  mutate(cell_lbl = sprintf('%s   (n_cex=%d, n_puf=%d)',
                            cell, n_cex, n_puf))

p = ggplot(ql, aes(x = p, y = q / 1000, color = source)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ cell_lbl, scales = 'free_y', ncol = 2) +
  scale_color_manual(values = c(CEX = '#2c7bb6', PUF = '#d7191c'),
                     name = NULL) +
  labs(title    = sprintf('F^-1(p) of C by cell — %d random cells with >=%d obs each side',
                          nrow(sampled), MIN_N),
       subtitle = 'Rank-binned (pctile) cells. Each line is one dataset; x=percentile, y=C in $k.',
       x = 'Percentile p', y = 'Quantile of C ($k, 2017)') +
  theme_minimal(base_size = 10) +
  theme(legend.position = 'top',
        strip.text      = element_text(size = 8),
        panel.grid.minor = element_blank())

out = 'plots/14_cell_quantile_sample_pctile.png'
ggsave(out, p, width = 11, height = 14, dpi = 140)
cat(sprintf('\nSaved: %s\n', out))
