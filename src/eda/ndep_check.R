lapply(readLines('requirements.txt'), library, character.only = TRUE)

puf_dem = read_rds('resources/cache/wealth_analysis.rds')

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')
y_vars = c(asset_vars, debt_vars)

compute_total = function(rds_path) {
  abl = read_rds(rds_path)
  donor_y = as_tibble(abl$donor_y)
  donor_y$id = abl$id
  merged = puf_dem %>% select(id, weight) %>%
    left_join(donor_y %>% select(id, all_of(y_vars)), by = 'id')
  nw = rowSums(merged[, asset_vars]) - rowSums(merged[, debt_vars])
  sum(nw * merged$weight) / 1e12
}

specs = list(
  'small'             = 'resources/cache/wealth_ablation_small.rds',
  'medium'            = 'resources/cache/wealth_ablation_medium.rds',
  'large'             = 'resources/cache/wealth_ablation_large.rds',
  'ladder10'          = 'resources/cache/wealth_ablation_ladder10.rds',
  'ladder10_no_ndep'  = 'resources/cache/wealth_ablation_ladder10_no_ndep.rds',
  'large_no_ndep'     = 'resources/cache/wealth_ablation_large_no_ndep.rds'
)

cat('Aggregate PUF NW across specs (SCF truth = $139.12T)\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')
cat(sprintf('  %-22s %10s %10s\n', 'spec', 'PUF ($T)', 'gap ($T)'))
for (nm in names(specs)) {
  tot = compute_total(specs[[nm]])
  cat(sprintf('  %-22s %10.2f %+10.2f\n', nm, tot, tot - 139.12))
}
