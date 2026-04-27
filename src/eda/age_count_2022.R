#---------------------------------------------------------------------
# age_count_2022.R
#
# Same goal as age_count_minimal.R but at 2022 (post-projection),
# matching the year the user's reference SCF/Census numbers describe.
# Skips Phase 3 (wealth imputation) since those are still in flux.
#
# Pipeline executed:
#   Phase 1 :  configure → process_targets → process_puf → reweight
#              defs → summary → create_2017_puf (full LP) →
#              impute_nonfilers → impute_variables (full)
#   Phase 2 :  project_puf.R (factor_ledger + weight_ledger to 2097)
#   Materialize at 2022 using base + ledgers, then senior diagnostics.
#---------------------------------------------------------------------

suppressPackageStartupMessages({
  invisible(lapply(readLines('requirements.txt'), library, character.only = TRUE))
})

cat('age_count_2022 start\n')

source('./src/configure.R')
set.seed(76)

# Phase 1
source('./src/process_targets.R'); cat('process_targets done\n')
source('./src/process_puf.R');     cat('process_puf done\n')
source('./src/reweight.R')
source('./src/summary.R')
source('./src/create_2017_puf.R'); cat('create_2017_puf done (LP)\n')
source('./src/impute_nonfilers.R'); cat('impute_nonfilers done\n')
source('./src/impute_variables.R'); cat('impute_variables done\n')

# Phase 2
source('./src/project_puf.R');     cat('project_puf done\n')

# Materialize 2022 (no wealth — module_deltas empty)
source('./src/materialize.R')
source('./src/record_bucket.R')
source('./src/dfa_factors.R')

puf_2022 = materialize(
  target_year   = 2022L,
  base          = tax_units,
  factor_ledger = factor_ledger,
  weight_ledger = weight_ledger,
  module_deltas = list()
)
cat(sprintf('materialized 2022: %d rows, %.2fM weight\n',
            nrow(puf_2022), sum(puf_2022$weight) / 1e6))

#---------------------------------------------------------------------
# Senior counts (mirrors senior_pop.R definitions)
#---------------------------------------------------------------------

td = puf_2022 %>%
  filter(dep_status == 0L) %>%
  mutate(
    age_older = if_else(is.na(age2), age1, pmax(age1, age2)),
    sen_tax   = age_older >= 65L,
    sen_pri   = age1 >= 65L,
    sen_sp    = !is.na(age2) & age2 >= 65L,
    both_sen  = filing_status == 2L & sen_pri & sen_sp,
    one_sen   = sen_tax & !both_sen
  )

total_units    = sum(td$weight) / 1e6
total_sen_unit = sum(td$weight[td$sen_tax]) / 1e6
sen_indiv      = (sum(td$weight[td$sen_pri]) +
                  sum(td$weight[td$sen_sp])) / 1e6
both_sen_M     = sum(td$weight[td$both_sen]) / 1e6
one_sen_M      = sum(td$weight[td$one_sen]) / 1e6

cat('\n=========================================\n')
cat('SENIOR COUNTS — PUF NEW 2022 (post-fix, post-projection)\n')
cat('=========================================\n')
cat(sprintf('Total tax units (non-dep) : %.2fM\n', total_units))
cat(sprintf('Senior tax units          : %.2fM\n', total_sen_unit))
cat(sprintf('Senior individuals        : %.2fM\n', sen_indiv))
cat(sprintf('  ...both spouses senior  : %.2fM\n', both_sen_M))
cat(sprintf('  ...only one senior      : %.2fM\n', one_sen_M))

cat('\n--- Side-by-side at year 2022 ---\n')
cat(sprintf('                              PUF NEW   PUF OLD   SCF       Census 2022\n'))
cat(sprintf('Total tax units              : %5.2fM   177.83M   170.37M   —\n',     total_units))
cat(sprintf('Senior tax units             : %5.2fM    46.53M    41.98M   —\n',      total_sen_unit))
cat(sprintf('Senior individuals           : %5.2fM    66.93M    54.59M   ~58M\n',   sen_indiv))
cat(sprintf('Both spouses senior          : %5.2fM    20.40M    12.61M   —\n',      both_sen_M))
cat(sprintf('Only one senior              : %5.2fM    26.13M    29.37M   —\n',      one_sen_M))

cat('\nDone.\n')
