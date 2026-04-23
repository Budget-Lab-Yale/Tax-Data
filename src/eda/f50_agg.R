lapply(readLines('requirements.txt'), library, character.only = TRUE)
puf_dem = read_rds('resources/cache/wealth_analysis.rds')

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')
y_vars = c(asset_vars, debt_vars)

compute_total = function(p) {
  if (!file.exists(p)) return(NA)
  abl = read_rds(p)
  dy = as_tibble(abl$donor_y); dy$id = abl$id
  m = puf_dem %>% select(id, weight) %>%
    left_join(dy %>% select(id, all_of(y_vars)), by = 'id')
  nw = rowSums(m[, asset_vars]) - rowSums(m[, debt_vars])
  sum(nw * m$weight) / 1e12
}

cat('\nD=10 baseline (num.features = 10, TRAIN varying, DONOR=1337):\n')
cat(sprintf('  %-8s %12s\n', 'seed', 'gap $T'))
d10_gaps = c()
for (ts in c(1337, 42, 2024, 7, 99)) {
  path = if (ts == 1337) 'resources/cache/wealth_ablation_large.rds'
         else sprintf('resources/cache/wealth_ablation_large_t%d_d1337.rds', ts)
  g = compute_total(path) - 139.12
  d10_gaps = c(d10_gaps, g)
  cat(sprintf('  %-8d %+12.2f\n', ts, g))
}
cat(sprintf('  mean=%.2f, sd=%.2f, range=%.2f\n',
            mean(d10_gaps), sd(d10_gaps), max(d10_gaps)-min(d10_gaps)))

cat('\nD=50 test (num.features = 50, TRAIN varying, DONOR=1337):\n')
cat(sprintf('  %-8s %12s\n', 'seed', 'gap $T'))
d50_gaps = c()
for (ts in c(1337, 42, 2024, 7, 99)) {
  path = sprintf('resources/cache/wealth_ablation_large_t%d_d1337_f50.rds', ts)
  g = compute_total(path) - 139.12
  d50_gaps = c(d50_gaps, g)
  cat(sprintf('  %-8d %+12.2f\n', ts, g))
}
cat(sprintf('  mean=%.2f, sd=%.2f, range=%.2f\n',
            mean(d50_gaps), sd(d50_gaps), max(d50_gaps)-min(d50_gaps)))

cat('\n=== Comparison ===\n')
cat(sprintf('  D=10: mean %.2f, sd %.2f\n', mean(d10_gaps), sd(d10_gaps)))
cat(sprintf('  D=50: mean %.2f, sd %.2f\n', mean(d50_gaps), sd(d50_gaps)))
cat(sprintf('  SD reduction: %.1fx\n', sd(d10_gaps) / sd(d50_gaps)))
