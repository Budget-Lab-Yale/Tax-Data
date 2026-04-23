#--------------------------------------
# ladder_aggregate.R
#
# Compute aggregate NW for each ladder spec
# by joining donor_y to PUF demographics
# and weights, then summing.
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)

puf_dem = read_rds('resources/cache/wealth_analysis.rds')

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')
y_vars = c(asset_vars, debt_vars)

results = tibble()
for (i in 1:10) {
  path = sprintf('resources/cache/wealth_ablation_ladder%d.rds', i)
  if (!file.exists(path)) {
    cat(sprintf('missing: %s\n', path)); next
  }
  abl = read_rds(path)
  donor_y = as_tibble(abl$donor_y)
  donor_y$id = abl$id
  merged = puf_dem %>% select(id, weight) %>%
    left_join(donor_y %>% select(id, all_of(y_vars)), by = 'id')
  nw = rowSums(merged[, asset_vars]) - rowSums(merged[, debt_vars])
  puf_total = sum(nw * merged$weight) / 1e12
  results = bind_rows(results, tibble(
    step = i,
    features = paste(abl$feats, collapse = ', '),
    n_feats = length(abl$feats),
    puf_total_tn = puf_total,
    gap_tn = puf_total - 139.12
  ))
}

results = results %>% mutate(marginal_change = gap_tn - lag(gap_tn))
cat('\n', strrep('=', 80), '\n', sep = '')
cat('Ladder: aggregate NW gap at each step\n')
cat(strrep('=', 80), '\n\n', sep = '')
cat(sprintf('  %-5s %4s %10s %10s %10s  %s\n',
            'step', '#ft', 'PUF $T', 'gap $T', 'Δ gap', 'features added'))
cat('  ', strrep('-', 100), '\n', sep = '')

prev_feats = character()
for (i in seq_len(nrow(results))) {
  cur_feats = strsplit(results$features[i], ', ')[[1]]
  added = setdiff(cur_feats, prev_feats)
  delta_str = if (is.na(results$marginal_change[i])) '' else
              sprintf('%+9.2f', results$marginal_change[i])
  cat(sprintf('  %-5d %4d %10.2f %+10.2f %10s  %s\n',
              results$step[i], results$n_feats[i],
              results$puf_total_tn[i], results$gap_tn[i],
              delta_str, paste(added, collapse = ', ')))
  prev_feats = cur_feats
}

write_csv(results, 'plots/wealth_decomp/ladder_aggregate.csv')
cat('\nwrote plots/wealth_decomp/ladder_aggregate.csv\n')
