#--------------------------------------
# check_cex_coverage.R
#
# Standalone check: run cex.R::build_cex_training() and compare the
# weighted aggregates of the 8 c_* categories to 2017-deflated NIPA
# PCE targets. Skips the full consumption imputation pipeline — we
# only need to verify that the CEX-side aggregates are now sensible
# after the HEALTHCQ split.
#
# Output: coverage table (CEX aggregate / NIPA target) per category.
#--------------------------------------

lapply(c('tidyverse', 'magrittr', 'data.table', 'Hmisc'),
       library, character.only = TRUE)

source('src/cex.R')
source('src/pce_benchmark.R')  # pce_collapse_map + pce_categories

cex = build_cex_training()
cat(sprintf('\nCEX training rows: %d\n', nrow(cex)))

# Weighted aggregates of the 8 c_* categories, billions (CEX is in 2017$)
pce_cats = c('c_clothing', 'c_motor_vehicles', 'c_durables',
             'c_other_nondurables', 'c_food_off_premises', 'c_gasoline',
             'c_housing_utilities', 'c_other_services_health')

agg_cex = sapply(pce_cats, function(cat) {
  sum(cex[[cat]] * cex$WT_ANNUAL, na.rm = TRUE) / 1e9
})

# NIPA targets (2023 CSV → deflate to 2017)
cpi_2023_to_2017 = 1.23825
fine = fread('resources/pce_targets_2023.csv')
fine$pce_billions_2017 = fine$pce_billions / cpi_2023_to_2017
fine_map = setNames(fine$pce_billions_2017, fine$category)
target_map = sapply(pce_collapse_map,
                    function(fcats) sum(fine_map[fcats], na.rm = TRUE))

cat('\n========== CEX → NIPA coverage (2017$, billions) ==========\n')
cat(sprintf('%-24s %12s %12s %10s %12s\n',
            'Category', 'CEX agg', 'NIPA 2017$', 'SF', 'Coverage'))
cat(paste0(rep('-', 76), collapse = ''), '\n')
for (cat_name in pce_cats) {
  cex_v = agg_cex[cat_name]
  nipa  = target_map[cat_name]
  sf    = nipa / cex_v
  cat(sprintf('%-24s %12.1f %12.1f %10.2f %11.1f%%\n',
              cat_name, cex_v, nipa, sf, cex_v / nipa * 100))
}

cat(sprintf('\nTotal CEX C:   $%7.1f B\n', sum(agg_cex)))
cat(sprintf('Total NIPA:    $%7.1f B\n', sum(target_map)))
cat(sprintf('Overall cov:   %.1f%%\n',
            sum(agg_cex) / sum(target_map) * 100))

cat('\n========== Done ==========\n')
