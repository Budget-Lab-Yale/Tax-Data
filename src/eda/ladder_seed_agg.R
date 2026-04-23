lapply(readLines('requirements.txt'), library, character.only = TRUE)

puf_dem = read_rds('resources/cache/wealth_analysis.rds')

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')
y_vars = c(asset_vars, debt_vars)

compute_total = function(rds_path) {
  if (!file.exists(rds_path)) return(NA)
  abl = read_rds(rds_path)
  donor_y = as_tibble(abl$donor_y)
  donor_y$id = abl$id
  merged = puf_dem %>% select(id, weight) %>%
    left_join(donor_y %>% select(id, all_of(y_vars)), by = 'id')
  nw = rowSums(merged[, asset_vars]) - rowSums(merged[, debt_vars])
  sum(nw * merged$weight) / 1e12
}

cat('\nLADDER (with n_dep_hh fix, seeds 1337/1337):\n')
cat(paste0(rep('-', 60), collapse = ''), '\n')
cat(sprintf('  %-10s %12s %12s\n', 'step', 'PUF $T', 'gap $T'))
prev_gap = NA
for (i in 1:10) {
  tot = compute_total(sprintf('resources/cache/wealth_ablation_ladder%d.rds', i))
  gap = tot - 139.12
  d = if (is.na(prev_gap)) '' else sprintf('%+8.2f', gap - prev_gap)
  cat(sprintf('  ladder%-4d %12.2f %+12.2f %s\n', i, tot, gap, d))
  prev_gap = gap
}

cat('\nLARGE — TRAIN seed variance (donor seed fixed at 1337):\n')
cat(paste0(rep('-', 60), collapse = ''), '\n')
cat(sprintf('  %-8s %8s %12s %12s\n', 'train', 'donor', 'PUF $T', 'gap $T'))
for (ts in c(1337, 42, 2024, 7, 99)) {
  path = if (ts == 1337) 'resources/cache/wealth_ablation_large.rds'
         else sprintf('resources/cache/wealth_ablation_large_t%d_d1337.rds', ts)
  tot = compute_total(path)
  cat(sprintf('  %-8d %8d %12.2f %+12.2f\n', ts, 1337, tot, tot - 139.12))
}

cat('\nLARGE — DONOR seed variance (train seed fixed at 1337):\n')
cat(paste0(rep('-', 60), collapse = ''), '\n')
cat(sprintf('  %-8s %8s %12s %12s\n', 'train', 'donor', 'PUF $T', 'gap $T'))
for (ds in c(1337, 42, 2024, 7, 99)) {
  path = if (ds == 1337) 'resources/cache/wealth_ablation_large.rds'
         else sprintf('resources/cache/wealth_ablation_large_t1337_d%d.rds', ds)
  tot = compute_total(path)
  cat(sprintf('  %-8d %8d %12.2f %+12.2f\n', 1337, ds, tot, tot - 139.12))
}

# Variance summary
cat('\n=== SUMMARY ===\n')
train_gaps = sapply(c(1337, 42, 2024, 7, 99), function(ts) {
  path = if (ts == 1337) 'resources/cache/wealth_ablation_large.rds'
         else sprintf('resources/cache/wealth_ablation_large_t%d_d1337.rds', ts)
  compute_total(path) - 139.12
})
donor_gaps = sapply(c(1337, 42, 2024, 7, 99), function(ds) {
  path = if (ds == 1337) 'resources/cache/wealth_ablation_large.rds'
         else sprintf('resources/cache/wealth_ablation_large_t1337_d%d.rds', ds)
  compute_total(path) - 139.12
})
cat(sprintf('TRAIN-seed MC: 5 gaps = [%s], mean=%.2f, sd=%.2f, range=%.2f\n',
            paste(sprintf('%.2f', train_gaps), collapse = ', '),
            mean(train_gaps), sd(train_gaps), max(train_gaps) - min(train_gaps)))
cat(sprintf('DONOR-seed MC: 5 gaps = [%s], mean=%.2f, sd=%.2f, range=%.2f\n',
            paste(sprintf('%.2f', donor_gaps), collapse = ', '),
            mean(donor_gaps), sd(donor_gaps), max(donor_gaps) - min(donor_gaps)))
