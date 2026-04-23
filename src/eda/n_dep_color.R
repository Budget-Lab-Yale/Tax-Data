#--------------------------------------
# n_dep_color.R
#
# Tabulate n_dep summary stats on PUF vs SCF across cuts.
# PUF: n_dep = tax-return dependent count (includes adults,
#   qualifying relatives, elderly parents, adult disabled
#   dependents). Has dep_age1, dep_age2, dep_age3 at row level.
# SCF: n_dep = KIDS_u = household kids from roster, split per
#   tax unit. Narrow. No adult dependents.
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)

report_path = 'plots/wealth_decomp/n_dep_color.txt'

scf      = read_rds('resources/cache/scf_tax_units.rds')
puf_dem  = read_rds('resources/cache/wealth_analysis.rds')
puf_full = read_rds('resources/cache/consumption_analysis.rds')

sink(report_path, split = TRUE)
cat('n_dep summary — PUF (tax-return dependents) vs SCF (household kids)\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 72), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# Universe note: wealth_analysis.rds has the full PUF universe including
# nonfilers. consumption_analysis.rds is PUF filtered to income>0 & C>0.
# For dep_age access, we need consumption_analysis.rds. For full-universe
# n_dep stats, we use wealth_analysis. We report both and note the
# universe mismatch.
#---------------------------------------------------------------------------

puf_univ_full = puf_dem %>%
  mutate(married = as.integer(married),
         n_dep = as.integer(n_dep))

puf_univ_withdep = puf_full %>%
  mutate(married = as.integer(married),
         n_dep = as.integer(n_dep))

scf_univ = scf %>%
  mutate(married = as.integer(married),
         n_dep = as.integer(n_dep))

w_share = function(mask, w) sum(w[mask], na.rm = TRUE) / sum(w, na.rm = TRUE)
w_mean  = function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

#---------------------------------------------------------------------------
# 1. Overall n_dep distribution (weighted).
#---------------------------------------------------------------------------

cat('1. Overall weighted n_dep distribution\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')
cat(sprintf('  %-8s %10s %10s %8s\n', 'n_dep', 'SCF %', 'PUF %', 'Δ'))
for (k in 0:5) {
  tag = if (k == 5) '5+' else as.character(k)
  s_mask = if (k < 5) scf_univ$n_dep == k else scf_univ$n_dep >= 5
  p_mask = if (k < 5) puf_univ_full$n_dep == k else puf_univ_full$n_dep >= 5
  s_sh = 100 * w_share(s_mask, scf_univ$weight)
  p_sh = 100 * w_share(p_mask, puf_univ_full$weight)
  cat(sprintf('  %-8s %9.1f%% %9.1f%% %+7.1f\n', tag, s_sh, p_sh, p_sh - s_sh))
}
cat(sprintf('\n  mean n_dep: SCF %.3f, PUF %.3f, Δ %+.3f\n\n',
            w_mean(scf_univ$n_dep, scf_univ$weight),
            w_mean(puf_univ_full$n_dep, puf_univ_full$weight),
            w_mean(puf_univ_full$n_dep, puf_univ_full$weight) -
              w_mean(scf_univ$n_dep, scf_univ$weight)))

#---------------------------------------------------------------------------
# 2. Mean n_dep by age band of primary filer.
#---------------------------------------------------------------------------

cat('2. Mean n_dep by age band of primary filer\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

for (d in list(scf_univ, puf_univ_full)) {
  d$age_b = cut(d$age1, c(-Inf, 35, 55, 65, Inf),
                labels = c('<35', '35-54', '55-64', '65+'), right = FALSE)
}
scf_univ$age_b = cut(scf_univ$age1, c(-Inf, 35, 55, 65, Inf),
                    labels = c('<35', '35-54', '55-64', '65+'), right = FALSE)
puf_univ_full$age_b = cut(puf_univ_full$age1, c(-Inf, 35, 55, 65, Inf),
                    labels = c('<35', '35-54', '55-64', '65+'), right = FALSE)

cat(sprintf('  %-10s %10s %10s %10s %10s %8s\n', 'age_band',
            'SCF mean', 'PUF mean', 'SCF n≥1%', 'PUF n≥1%', 'Δ n≥1%'))
for (ab in c('<35', '35-54', '55-64', '65+')) {
  sm = w_mean(scf_univ$n_dep[scf_univ$age_b == ab],
              scf_univ$weight[scf_univ$age_b == ab])
  pm = w_mean(puf_univ_full$n_dep[puf_univ_full$age_b == ab],
              puf_univ_full$weight[puf_univ_full$age_b == ab])
  ss = 100 * w_share((scf_univ$age_b == ab) & (scf_univ$n_dep >= 1),
                     scf_univ$weight) /
         w_share(scf_univ$age_b == ab, scf_univ$weight)
  ps = 100 * w_share((puf_univ_full$age_b == ab) & (puf_univ_full$n_dep >= 1),
                     puf_univ_full$weight) /
         w_share(puf_univ_full$age_b == ab, puf_univ_full$weight)
  cat(sprintf('  %-10s %10.3f %10.3f %9.1f%% %9.1f%% %+7.1f\n',
              ab, sm, pm, ss, ps, ps - ss))
}

#---------------------------------------------------------------------------
# 3. Joint age × married.
#---------------------------------------------------------------------------

cat('\n3. Share with n_dep ≥ 1, by age × marital\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')
cat(sprintf('  %-10s %-8s %10s %10s %8s\n', 'age', 'mar', 'SCF %', 'PUF %', 'Δ'))
for (ab in c('<35', '35-54', '55-64', '65+')) {
  for (mr in c(1, 0)) {
    mtag = if (mr == 1) 'mar' else 'sin'
    mask_s = scf_univ$age_b == ab & scf_univ$married == mr
    mask_p = puf_univ_full$age_b == ab & puf_univ_full$married == mr
    ss = 100 * w_share(mask_s & scf_univ$n_dep >= 1, scf_univ$weight) /
              w_share(mask_s, scf_univ$weight)
    ps = 100 * w_share(mask_p & puf_univ_full$n_dep >= 1, puf_univ_full$weight) /
              w_share(mask_p, puf_univ_full$weight)
    cat(sprintf('  %-10s %-8s %9.1f%% %9.1f%% %+7.1f\n',
                ab, mtag, ss, ps, ps - ss))
  }
}

#---------------------------------------------------------------------------
# 4. PUF ONLY: what AGES are those dependents?
#    Using dep_age_group1/2/3. Groups:
#      1 = 0-5    2 = 5-13   3 = 13-17
#      4 = 17-19  5 = 19-24  6 = 24+  (ADULT)
#    The key diagnostic: among 65+ households with n_dep ≥ 1, what share
#    have at least one ADULT (group 6) dependent?
#---------------------------------------------------------------------------

cat('\n4. PUF dep-age breakdown (consumption_analysis.rds universe)\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')
cat('  Among PUF tax units with n_dep >= 1, distribution of each dep age group:\n\n')

# dep_age_group values — check which are present
for (gcol in c('dep_age_group1', 'dep_age_group2', 'dep_age_group3')) {
  if (!gcol %in% names(puf_univ_withdep)) {
    cat(sprintf('  %s: column not in consumption_analysis.rds\n', gcol))
    next
  }
}

# Use dep_age1/2/3 (imputed ages from groups)
if (all(c('dep_age1', 'dep_age2', 'dep_age3') %in% names(puf_univ_withdep))) {
  puf_univ_withdep$age_b = cut(puf_univ_withdep$age1, c(-Inf, 35, 55, 65, Inf),
                               labels = c('<35', '35-54', '55-64', '65+'),
                               right = FALSE)

  # For each PUF unit, check if any dep is 18+
  d = puf_univ_withdep %>%
    mutate(
      has_kid_u18   = coalesce(dep_age1 < 18, FALSE) |
                      coalesce(dep_age2 < 18, FALSE) |
                      coalesce(dep_age3 < 18, FALSE),
      has_adult_dep = coalesce(dep_age1 >= 18, FALSE) |
                      coalesce(dep_age2 >= 18, FALSE) |
                      coalesce(dep_age3 >= 18, FALSE),
      has_elderly_dep = coalesce(dep_age1 >= 65, FALSE) |
                        coalesce(dep_age2 >= 65, FALSE) |
                        coalesce(dep_age3 >= 65, FALSE)
    )

  cat(sprintf('  %-10s %10s %10s %10s %10s\n', 'age_band',
              'any_dep%', 'kid_u18%', 'adult_dep%', 'elderly_dep%'))
  for (ab in c('<35', '35-54', '55-64', '65+')) {
    mask = d$age_b == ab
    any_dep     = 100 * w_share(mask & d$n_dep >= 1, d$weight) /
                  w_share(mask, d$weight)
    has_kid     = 100 * w_share(mask & d$has_kid_u18, d$weight) /
                  w_share(mask, d$weight)
    has_adult   = 100 * w_share(mask & d$has_adult_dep, d$weight) /
                  w_share(mask, d$weight)
    has_elderly = 100 * w_share(mask & d$has_elderly_dep, d$weight) /
                  w_share(mask, d$weight)
    cat(sprintf('  %-10s %9.1f%% %9.1f%% %9.1f%% %9.1f%%\n',
                ab, any_dep, has_kid, has_adult, has_elderly))
  }

  cat('\n  Within 65+ married with n_dep ≥ 1 on PUF:\n')
  e = d %>% filter(age_b == '65+', married == 1, n_dep >= 1)
  cat(sprintf('    weighted households:          %.2fM\n', sum(e$weight) / 1e6))
  cat(sprintf('    share with a kid under 18:    %5.1f%%\n',
              100 * w_share(e$has_kid_u18, e$weight)))
  cat(sprintf('    share with an adult dep ≥18:  %5.1f%%\n',
              100 * w_share(e$has_adult_dep, e$weight)))
  cat(sprintf('    share with an elderly dep ≥65: %5.1f%%\n',
              100 * w_share(e$has_elderly_dep, e$weight)))

  # Mean dep_age among 65+ w/ n_dep>=1
  all_dep_ages = c(e$dep_age1, e$dep_age2, e$dep_age3)
  all_wt       = rep(e$weight, 3)
  keep         = !is.na(all_dep_ages)
  cat(sprintf('    mean dep age:                 %.1f\n',
              weighted.mean(all_dep_ages[keep], all_wt[keep])))

  # Distribution of dep ages among 65+ married w/ dep
  cat('\n    dep-age distribution (each dep_age slot, weighted by tu weight):\n')
  brks = c(0, 18, 25, 45, 65, 100)
  tags = c('0-17', '18-24', '25-44', '45-64', '65+')
  for (b in seq_len(length(brks) - 1)) {
    mask = !is.na(all_dep_ages) & all_dep_ages >= brks[b] & all_dep_ages < brks[b+1]
    sh = sum(all_wt[mask]) / sum(all_wt[!is.na(all_dep_ages)])
    cat(sprintf('      %-8s %6.1f%%\n', tags[b], 100 * sh))
  }
}

#---------------------------------------------------------------------------
# 5. SCF equivalent: KIDSU18 by age band (was SCF restricting to kids?)
#---------------------------------------------------------------------------

cat('\n5. SCF sanity: does n_dep match KIDSU17 on SCF?\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')
cat('  (If n_dep = KIDS_u from household roster, it should match KIDSU17 or similar)\n')
# scf_tax_units was built from a transmute so raw KIDS columns may not persist
# Just check n_dep range on SCF
cat(sprintf('  SCF n_dep range: %d to %d; mean %.2f; share >= 1: %.1f%%\n',
            min(scf_univ$n_dep), max(scf_univ$n_dep),
            w_mean(scf_univ$n_dep, scf_univ$weight),
            100 * w_share(scf_univ$n_dep >= 1, scf_univ$weight)))

cat(sprintf('  PUF n_dep range: %d to %d; mean %.2f; share >= 1: %.1f%%\n',
            min(puf_univ_full$n_dep), max(puf_univ_full$n_dep),
            w_mean(puf_univ_full$n_dep, puf_univ_full$weight),
            100 * w_share(puf_univ_full$n_dep >= 1, puf_univ_full$weight)))

cat('\n\nArtifacts: plots/wealth_decomp/n_dep_color.txt\n')
sink()
