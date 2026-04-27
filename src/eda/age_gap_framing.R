#----------------------------------------------------------------------
# age_gap_framing.R
#
# Diagnostic for the senior-individual over-count traced to ages.R.
#
# ages.R draws age2 from CPS conditional P(spouse_age | ref_person_age),
# treating PUF "primary" as if it were the CPS reference person. Two
# things could be biased relative to the true married-couple age dist:
#
#   (1) CPS reference person ≠ random married adult: ref person tends
#       to be the older / male spouse, so P(spouse | ref=A) is more
#       "older × older" than P(spouse | random member=A).
#   (2) CPS topcodes age at 80, smashing the senior tail and
#       redistributing 80-95 partners into the age=80 bucket.
#
# This script compares three CPS-derived conditionals against SCF
# truth, on the universe of married couples:
#
#   (a) Asymmetric  : current ages.R approach (ref → spouse).
#   (b) Symmetric   : pool {ref→spouse, spouse→ref} as ego→other.
#   (c) Symmetric+sex: condition on ego sex.
#
# We then simulate the age2|age1 step on SCF senior-MFJ couples using
# each CPS conditional and compare the implied both-senior share to
# SCF empirical truth.
#----------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(haven)
  library(stringr); library(tibble); library(magrittr)
})

set.seed(20260427)

CPS_PATH = '/nfs/roberts/project/pi_nrs36/shared/raw_data/CPS-ASEC/v1/2024110517/historical/cps_00027.csv.gz'
SCF_PATH = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/2022/historical/p22i6.dta'

TOPCODE = 80L
SENIOR  = 65L

#-------------------- 1. CPS married couples --------------------------

cat('Loading CPS-ASEC ...\n')
cps_raw = read_csv(CPS_PATH, show_col_types = FALSE)
cat(sprintf('  %d CPS person-rows\n', nrow(cps_raw)))

# Extract householder (0101) + spouse-of-householder (0201) pairs.
# Same restriction as ages.R: drop CPSID==0, age >= 18.
cps_pairs = cps_raw %>%
  filter(RELATE %in% c(0101, 0201), CPSID != 0, AGE >= 18) %>%
  mutate(role = if_else(RELATE == 0101, 'ref', 'spouse')) %>%
  select(YEAR, CPSID, role, age = AGE, sex = SEX, weight = ASECWTH) %>%
  pivot_wider(
    names_from  = role,
    values_from = c(age, sex)
  ) %>%
  filter(!is.na(age_ref), !is.na(age_spouse))   # only complete couples

stopifnot(!any(is.na(cps_pairs$weight)))

cat(sprintf('  %d CPS couples (ref+spouse both present)\n', nrow(cps_pairs)))
cat(sprintf('  weighted: %.2fM\n', sum(cps_pairs$weight) / 1e6))

# topcode at 80 (matches ages.R)
cps_pairs = cps_pairs %>%
  mutate(
    age_ref_tc    = pmin(age_ref,    TOPCODE),
    age_spouse_tc = pmin(age_spouse, TOPCODE)
  )

# Sex check: how often is ref person male in CPS couples?
ref_male_share = with(cps_pairs, weighted.mean(sex_ref == 1, weight))
cat(sprintf('  CPS: share of married couples where reference person is male: %.1f%%\n',
            100 * ref_male_share))

#-------------------- 2. SCF married couples --------------------------

cat('\nLoading raw SCF ...\n')
scf_raw = haven::read_dta(SCF_PATH)
names(scf_raw) = toupper(names(scf_raw))

# Married = X7020 == 2 (partner present in PEU). We use SCF's own coupling.
scf_pairs = scf_raw %>%
  transmute(
    Y1,
    age_head = as.integer(X14),         # head age
    age_sp   = as.integer(X19),         # spouse age (0 if no spouse)
    sex_head = as.integer(X8021),       # 1 = male, 2 = female
    sex_sp   = as.integer(X103),        # 1 / 2
    has_sp   = as.integer(X7020 == 2),
    weight   = X42001 / 5
  ) %>%
  filter(has_sp == 1, age_head >= 18, age_sp >= 18)

stopifnot(!any(is.na(scf_pairs$weight)))

cat(sprintf('  %d SCF couple-rows (5 implicates)\n', nrow(scf_pairs)))
cat(sprintf('  weighted: %.2fM\n', sum(scf_pairs$weight) / 1e6))

# topcode at 80 to compare apples-to-apples
scf_pairs = scf_pairs %>%
  mutate(
    age_head_tc = pmin(age_head, TOPCODE),
    age_sp_tc   = pmin(age_sp,   TOPCODE)
  )

scf_head_male = with(scf_pairs, weighted.mean(sex_head == 1, weight))
cat(sprintf('  SCF: share where head is male: %.1f%%\n', 100 * scf_head_male))

#-------------------- 3. Build conditional summaries ------------------

# Helper: stack a paired tibble into ego/other rows (each row contributes twice).
stack_symmetric = function(df, age_a, age_b, sex_a, sex_b, weight) {
  df_a = df %>% transmute(ego_age   = .data[[age_a]],
                          other_age = .data[[age_b]],
                          ego_sex   = .data[[sex_a]],
                          other_sex = .data[[sex_b]],
                          weight    = .data[[weight]])
  df_b = df %>% transmute(ego_age   = .data[[age_b]],
                          other_age = .data[[age_a]],
                          ego_sex   = .data[[sex_b]],
                          other_sex = .data[[sex_a]],
                          weight    = .data[[weight]])
  bind_rows(df_a, df_b)
}

cps_asym = cps_pairs %>%
  transmute(ego_age   = age_ref_tc,
            other_age = age_spouse_tc,
            ego_sex   = sex_ref,
            other_sex = sex_spouse,
            weight)

cps_sym  = stack_symmetric(cps_pairs, 'age_ref_tc', 'age_spouse_tc',
                                       'sex_ref',    'sex_spouse', 'weight')

scf_asym = scf_pairs %>%
  transmute(ego_age   = age_head_tc,
            other_age = age_sp_tc,
            ego_sex   = sex_head,
            other_sex = sex_sp,
            weight)

scf_sym  = stack_symmetric(scf_pairs, 'age_head_tc', 'age_sp_tc',
                                       'sex_head',    'sex_sp', 'weight')

#----- 3a. P(other_age >= 65 | ego_age = a), at a few points ---------

probe_ages = c(60, 65, 70, 75, 80)

cond_p_other_senior = function(df, ego_age) {
  s = df %>% filter(ego_age == !!ego_age)
  if (nrow(s) == 0) return(NA_real_)
  weighted.mean(s$other_age >= SENIOR, s$weight)
}

cat('\n--- P(spouse senior | ego_age = a), topcoded at 80 ---\n')
tab_pt = tibble(ego_age = probe_ages) %>%
  rowwise() %>%
  mutate(
    cps_asym  = cond_p_other_senior(cps_asym, ego_age),
    cps_sym   = cond_p_other_senior(cps_sym,  ego_age),
    scf_asym  = cond_p_other_senior(scf_asym, ego_age),
    scf_sym   = cond_p_other_senior(scf_sym,  ego_age),
  ) %>%
  ungroup()
print(tab_pt %>% mutate(across(starts_with(c('cps','scf')), ~ round(., 3))))

#----- 3b. Same but conditioning on ego sex --------------------------

cond_p_other_senior_sex = function(df, ego_age, sex) {
  s = df %>% filter(ego_age == !!ego_age, ego_sex == !!sex)
  if (nrow(s) == 0) return(NA_real_)
  weighted.mean(s$other_age >= SENIOR, s$weight)
}

cat('\n--- P(spouse senior | ego_age = a, ego_sex), topcoded ---\n')
tab_sex = tibble(ego_age = probe_ages) %>%
  rowwise() %>%
  mutate(
    cps_male   = cond_p_other_senior_sex(cps_sym, ego_age, 1L),
    cps_female = cond_p_other_senior_sex(cps_sym, ego_age, 2L),
    scf_male   = cond_p_other_senior_sex(scf_sym, ego_age, 1L),
    scf_female = cond_p_other_senior_sex(scf_sym, ego_age, 2L)
  ) %>% ungroup()
print(tab_sex %>% mutate(across(-ego_age, ~ round(., 3))))

#----- 3c. Conditional mean / sd of other_age | ego_age --------------

cond_moments = function(df, ego_age) {
  s = df %>% filter(ego_age == !!ego_age)
  if (nrow(s) == 0) return(c(NA, NA))
  m  = weighted.mean(s$other_age, s$weight)
  v  = weighted.mean((s$other_age - m)^2, s$weight)
  c(m, sqrt(v))
}

cat('\n--- Mean / SD of spouse age | ego_age (topcoded) ---\n')
tab_mom = tibble(ego_age = probe_ages) %>%
  rowwise() %>%
  mutate(
    cps_asym_m = cond_moments(cps_asym, ego_age)[1],
    cps_asym_s = cond_moments(cps_asym, ego_age)[2],
    cps_sym_m  = cond_moments(cps_sym,  ego_age)[1],
    cps_sym_s  = cond_moments(cps_sym,  ego_age)[2],
    scf_sym_m  = cond_moments(scf_sym,  ego_age)[1],
    scf_sym_s  = cond_moments(scf_sym,  ego_age)[2]
  ) %>% ungroup()
print(tab_mom %>% mutate(across(-ego_age, ~ round(., 2))))

#----- 3d. Marginal: P(other senior | ego senior) --------------------

p_both_senior_marginal = function(df) {
  s = df %>% filter(ego_age >= SENIOR)
  weighted.mean(s$other_age >= SENIOR, s$weight)
}

cat('\n--- Marginal P(spouse senior | ego senior), topcoded ---\n')
cat(sprintf('  CPS asymmetric (ages.R)    : %.3f\n', p_both_senior_marginal(cps_asym)))
cat(sprintf('  CPS symmetric              : %.3f\n', p_both_senior_marginal(cps_sym)))
cat(sprintf('  SCF asymmetric (head→sp)   : %.3f\n', p_both_senior_marginal(scf_asym)))
cat(sprintf('  SCF symmetric (truth)      : %.3f\n', p_both_senior_marginal(scf_sym)))

#-------------------- 4. End-to-end simulation ------------------------
#
# For each SCF senior-MFJ couple (ego_age >= 66 to mirror band 6, but
# we'll also do ego_age >= 65), draw a partner age via each CPS
# conditional and compute the resulting share of "both senior". This
# isolates the impact of swapping the conditional in ages.R, holding
# the senior MFJ population fixed at SCF's actual (age, sex) marginal.
#
# We pre-build empirical conditional pmfs at integer ego_age levels.

build_pmf = function(df, with_sex = FALSE) {
  if (with_sex) {
    df %>%
      group_by(ego_age, ego_sex, other_age) %>%
      summarise(w = sum(weight), .groups = 'drop') %>%
      group_by(ego_age, ego_sex) %>%
      mutate(p = w / sum(w)) %>%
      ungroup() %>%
      select(ego_age, ego_sex, other_age, p)
  } else {
    df %>%
      group_by(ego_age, other_age) %>%
      summarise(w = sum(weight), .groups = 'drop') %>%
      group_by(ego_age) %>%
      mutate(p = w / sum(w)) %>%
      ungroup() %>%
      select(ego_age, other_age, p)
  }
}

pmf_cps_asym = build_pmf(cps_asym)
pmf_cps_sym  = build_pmf(cps_sym)
pmf_cps_sex  = build_pmf(cps_sym, with_sex = TRUE)
pmf_scf_sym  = build_pmf(scf_sym)        # ground truth

# Sample one other_age per row of `egos` (a tibble with columns ego_age,
# weight, optionally ego_sex), drawing from `pmf` matched on those keys.
sample_other = function(egos, pmf, with_sex = FALSE) {
  key = if (with_sex) c('ego_age', 'ego_sex') else 'ego_age'
  joined = egos %>%
    mutate(.row = row_number()) %>%
    left_join(pmf, by = key, relationship = 'many-to-many')
  # row-level sampling: one draw per .row weighted by p
  joined %>%
    group_by(.row) %>%
    slice_sample(n = 1, weight_by = p) %>%
    ungroup() %>%
    pull(other_age)
}

# Senior couples in SCF (older spouse >= 65). For each, ego is one of
# the partners; we set the ego to the OLDER spouse to mirror PUF
# AGERANGE = primary's age (which in IRS convention is often the older /
# male partner; see ref_male_share above). We hold ego_age fixed and
# resample the other partner age via each conditional.

scf_senior_couples = scf_pairs %>%
  mutate(
    older_age = pmax(age_head_tc, age_sp_tc),
    older_sex = if_else(age_head_tc >= age_sp_tc, sex_head, sex_sp),
    younger_age = pmin(age_head_tc, age_sp_tc)
  ) %>%
  filter(older_age >= SENIOR) %>%
  mutate(ego_age = older_age, ego_sex = older_sex)

cat(sprintf('\nSCF senior MFJ couples: n=%d, weighted=%.2fM\n',
            nrow(scf_senior_couples),
            sum(scf_senior_couples$weight) / 1e6))

scf_truth_both = with(scf_senior_couples,
                      weighted.mean(younger_age >= SENIOR, weight))

# Each draw is stochastic; do a small number of replicates and average.
n_rep = 5
sim_one = function(pmf, with_sex = FALSE) {
  shares = replicate(n_rep, {
    other = sample_other(scf_senior_couples %>%
                           select(ego_age, ego_sex, weight),
                         pmf, with_sex = with_sex)
    weighted.mean(other >= SENIOR, scf_senior_couples$weight)
  })
  c(mean = mean(shares), sd = sd(shares))
}

cat('\n--- Simulated P(other senior | older partner senior), n_rep=5 ---\n')
cat(sprintf('  SCF empirical truth (no sampling): %.3f\n', scf_truth_both))

s = sim_one(pmf_cps_asym, with_sex = FALSE)
cat(sprintf('  CPS asymmetric (current ages.R)  : %.3f  (sd %.4f)\n', s['mean'], s['sd']))

s = sim_one(pmf_cps_sym, with_sex = FALSE)
cat(sprintf('  CPS symmetric                    : %.3f  (sd %.4f)\n', s['mean'], s['sd']))

s = sim_one(pmf_cps_sex, with_sex = TRUE)
cat(sprintf('  CPS symmetric + ego sex          : %.3f  (sd %.4f)\n', s['mean'], s['sd']))

s = sim_one(pmf_scf_sym, with_sex = FALSE)
cat(sprintf('  SCF symmetric (sanity-check)     : %.3f  (sd %.4f)\n', s['mean'], s['sd']))

#-------------------- 5. Off-by-one band check ------------------------
#
# IRS PUF AGERANGE band 6 is documented as "65 and over"; ages.R treats
# band 6 as 66+. Quantify the share of CPS adults aged 65 (single year)
# inside what ages.R calls band 5 (56-65).

age_share_65 = cps_raw %>%
  filter(AGE >= 18) %>%
  summarise(p65 = weighted.mean(AGE == 65, ASECWT),
            p55_65 = weighted.mean(AGE >= 55 & AGE <= 65, ASECWT),
            p66_plus = weighted.mean(AGE >= 66, ASECWT))
cat('\n--- Age 65 mass in CPS adult population ---\n')
print(age_share_65)
cat(sprintf('  share of band-5 (56-65) that is exactly 65: %.3f\n',
            with(age_share_65, p65 / p55_65)))

cat('\nDone.\n')
