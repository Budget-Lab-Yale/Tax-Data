#---------------------------------------------------------------------
# age_count_minimal.R
#
# Minimal slice of the pipeline that produces post-LP, post-nonfiler,
# post-ages tax_units, and reports senior counts comparable to
# senior_pop.R. Skips dollar-amount rescaling, projection, and other
# imputations (none of which affect age structure at 2017 base year).
#
# Order:
#   configure → process_targets → process_puf → reweight (defs) →
#   create_2017_puf (just the LP weights step) → impute_nonfilers →
#   ages.R → compute senior counts.
#---------------------------------------------------------------------

suppressPackageStartupMessages({
  invisible(lapply(readLines('requirements.txt'), library, character.only = TRUE))
})

cat('age_count_minimal start\n')

source('./src/configure.R')
set.seed(76)

source('./src/process_targets.R')
cat('process_targets done\n')

source('./src/process_puf.R')
cat('process_puf done\n')

source('./src/reweight.R')
source('./src/summary.R')

# Replicate the weight-affecting subset of create_2017_puf.R (lines 13-57):
# pre-LP scaling → reweight_lp → apply weight_deltas. We skip the
# subsequent dollar-amount rescaling block (lines 60-137) since it
# does not touch weights or ages.

puf %<>%
  mutate(married = as.integer(filing_status == 2),
         senior  = as.integer(age_group == 6))

weight_scaling_factors = tables$table_1_6 %>%
  filter(year == 2017) %>%
  group_by(married   = as.integer(filing_status == 2),
           senior    = as.integer(age_group == 6),
           agi_group = agi) %>%
  summarise(count2017 = sum(count), .groups = 'drop') %>%
  left_join(puf %>%
              group_by(married, senior, agi_group) %>%
              summarise(puf_w = sum(weight), .groups = 'drop'),
            by = c('married', 'senior', 'agi_group')) %>%
  mutate(factor = count2017 / puf_w) %>%
  select(-count2017, -puf_w)

puf_2017 = puf %>%
  left_join(weight_scaling_factors, by = c('married', 'senior', 'agi_group')) %>%
  mutate(weight = weight * factor) %>%
  select(-factor)

cat(sprintf('pre-LP: puf_2017 weight=%.2fM\n', sum(puf_2017$weight) / 1e6))

# Build targets and run LP fresh (do_lp = TRUE here).
targets = target_info %>%
  bind_cols(
    purrr::map(.x = 1:nrow(target_info),
               .f = ~ get_target_value(target_info[.x, ])) %>%
      bind_rows()
  )
cat(sprintf('targets: %d constraints\n', nrow(targets)))

weight_deltas = reweight_lp(puf_2017, targets, do_lp = TRUE, e = 0.66)
puf_2017 = puf_2017 %>% mutate(weight = weight * weight_deltas)
cat(sprintf('post-LP: puf_2017 weight=%.2fM\n', sum(puf_2017$weight) / 1e6))

# Now run impute_nonfilers (creates `tax_units`)
source('./src/impute_nonfilers.R')
cat(sprintf('post-nonfilers: tax_units rows=%d, weight=%.2fM\n',
            nrow(tax_units), sum(tax_units$weight) / 1e6))

# ages.R needs `interface_paths` (CPS path) and `tax_units` with
# `age_group`, `filing_status`, `id`, `dep_age_group{1,2,3}`. All set.
source('./src/imputations/ages.R')
cat(sprintf('post-ages: tax_units has age1 (n na = %d), age2 (n na = %d, MFJ na = %d)\n',
            sum(is.na(tax_units$age1)),
            sum(is.na(tax_units$age2)),
            sum(is.na(tax_units$age2) & tax_units$filing_status == 2L)))

#---------------------------------------------------------------------
# Senior counts (matches senior_pop.R definitions)
#---------------------------------------------------------------------

cat('\n=========================================\n')
cat('SENIOR COUNTS (post-LP, post-ages, no projection)\n')
cat('=========================================\n')

# ages.R drops age_group; mimic project_puf.R's re-binning rule from age1
band_of = function(age) dplyr::case_when(
  age < 26 ~ 1L, age < 35 ~ 2L, age < 45 ~ 3L,
  age < 55 ~ 4L, age < 65 ~ 5L, TRUE     ~ 6L
)

td = tax_units %>%
  filter(dep_status == 0L) %>%
  mutate(
    age_group   = band_of(age1),
    age_older   = if_else(is.na(age2), age1, pmax(age1, age2)),
    sen_tax     = age_older >= 65L,
    sen_pri     = age1 >= 65L,
    sen_sp      = !is.na(age2) & age2 >= 65L,
    both_sen    = filing_status == 2L & sen_pri & sen_sp,
    one_sen     = sen_tax & !both_sen
  )

total_units    = sum(td$weight) / 1e6
total_sen_unit = sum(td$weight[td$sen_tax]) / 1e6
sen_indiv      = (sum(td$weight[td$sen_pri]) +
                  sum(td$weight[td$sen_sp])) / 1e6
both_sen_M     = sum(td$weight[td$both_sen]) / 1e6
one_sen_M      = sum(td$weight[td$one_sen]) / 1e6

cat(sprintf('Total tax units (non-dep) : %.2fM\n', total_units))
cat(sprintf('Senior tax units (older>=65): %.2fM\n', total_sen_unit))
cat(sprintf('Senior individuals          : %.2fM\n', sen_indiv))
cat(sprintf('  ...both spouses senior    : %.2fM\n', both_sen_M))
cat(sprintf('  ...only one senior        : %.2fM\n', one_sen_M))

# Filer-only subset for SOI cross-check
tdf = td %>% filter(filer == 1L)
cat('\n--- Filers only (filer==1) ---\n')
cat(sprintf('Filers total                : %.2fM\n', sum(tdf$weight) / 1e6))
cat(sprintf('Filers MFJ + age_group 6    : %.2fM   (SOI 2017 = 13.20M)\n',
            sum(tdf$weight[tdf$filing_status == 2L & tdf$age_group == 6L]) / 1e6))
cat(sprintf('Filers single + age_group 6 : %.2fM   (SOI 2017 = 11.69M)\n',
            sum(tdf$weight[tdf$filing_status == 1L & tdf$age_group == 6L]) / 1e6))
cat(sprintf('Filers all + age_group 1    : %.2fM   (SOI 2017 = 25.59M)\n',
            sum(tdf$weight[tdf$age_group == 1L]) / 1e6))
cat(sprintf('Filers all + age_group 6    : %.2fM   (SOI 2017 = 25.97M)\n',
            sum(tdf$weight[tdf$age_group == 6L]) / 1e6))

# (fs × ag) cell breakdown for filers
cell_breakdown = tdf %>%
  group_by(filing_status, age_group) %>%
  summarise(weight_M = sum(weight) / 1e6, .groups = 'drop') %>%
  arrange(filing_status, age_group)

cat('\n--- Filer cells (filing_status × age_group, M) ---\n')
print(cell_breakdown, n = Inf)

# Side-by-side vs the user's table (PUF projected 2022 / SCF / Census 2022)
cat('\n--- Headline comparison (2017 base) ---\n')
cat(sprintf('                              PUF(NEW)   PUF(OLD 2022)   SCF(2022)   Census 2022\n'))
cat(sprintf('Total tax units              : %5.2fM    177.83M         170.37M     —\n',
            total_units))
cat(sprintf('Senior tax units             : %5.2fM     46.53M          41.98M     —\n',
            total_sen_unit))
cat(sprintf('Senior individuals           : %5.2fM     66.93M          54.59M     ~58M\n',
            sen_indiv))
cat(sprintf('Both-spouses senior          : %5.2fM     20.40M          12.61M     —\n',
            both_sen_M))
cat(sprintf('Only one senior              : %5.2fM     26.13M          29.37M     —\n',
            one_sen_M))
cat('  (NEW = post-fix, 2017 base; OLD = current pipeline output for 2022)\n')

cat('\nDone.\n')
