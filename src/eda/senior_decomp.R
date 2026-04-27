#---------------------------------------------------------------------
# senior_decomp.R
#
# Decompose PUF senior individuals into three masses to localize the
# source of the senior over-count vs SCF / Census:
#
#   Mass A  MFJ tax units with primary's age_group = 6 (primary >= 65).
#           SOI Table 1.6 keys on primary's age, so this count is the
#           one anchored by reweighting. Individuals contributed:
#           primary (always >= 65) + spouse (>= 65 by spouse imputation
#           with prob ~0.8 conditional on primary in band 6).
#
#   Mass B  MFJ tax units with primary's age_group <= 5 (primary < 65)
#           but spouse imputed >= 65. NOT locked by SOI 1.6 — driven
#           entirely by the spouse-age conditional in ages.R.
#
#   Mass C  Single-filer (non-MFJ) tax units with primary >= 65. Pure
#           primary-age draw; no spouse imputation involved.
#
# Cross-check: PUF MFJ count where primary in band 6 vs SOI 1.6 row sum
# for (filing_status=2, age_group=6) at 2017 (latest year compiled).
#---------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble)
})

PUF_2022 = '/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline/tax_units_2022.csv'
PUF_2017 = '/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline/tax_units_2017.csv'
SOI_TBL  = '/nfs/roberts/project/pi_nrs36/shared/model_data/Compiled-SOI-Tables/v2/2023100323/historical/table_1_6.csv'

SENIOR = 65L

# project_puf.R band cuts (note: < not <=, so band 6 = age >= 65)
band_of = function(age) {
  case_when(
    age < 26 ~ 1L,
    age < 35 ~ 2L,
    age < 45 ~ 3L,
    age < 55 ~ 4L,
    age < 65 ~ 5L,
    TRUE     ~ 6L
  )
}

decompose = function(path, label) {
  cat(sprintf('\n=== %s : %s ===\n', label, path))
  df = read_csv(path, show_col_types = FALSE) %>%
    filter(dep_status == 0L) %>%
    select(id, weight, filing_status, age1, age2, filer)

  stopifnot(!any(is.na(df$age1)), !any(is.na(df$weight)),
            !any(is.na(df$filing_status)), !any(is.na(df$filer)))

  df = df %>%
    mutate(
      age_group  = band_of(age1),
      mfj        = filing_status == 2L,
      pri_senior = age1 >= SENIOR,
      sp_senior  = !is.na(age2) & age2 >= SENIOR,
      both_sen   = mfj & pri_senior & sp_senior,
      is_filer   = filer == 1L
    )

  cat(sprintf('  filer / non-filer split: %.2fM filers + %.2fM non-filers\n',
              sum(df$weight[df$is_filer]) / 1e6,
              sum(df$weight[!df$is_filer]) / 1e6))

  total_seniors_indiv = with(df,
    sum(weight[pri_senior]) + sum(weight[sp_senior])
  )
  total_senior_taxunits = with(df,
    sum(weight[pri_senior | sp_senior])
  )

  # Mass A: MFJ + primary >= 65. Individual count = primary + spouse-if-senior
  A_units      = with(df, sum(weight[mfj & pri_senior]))
  A_indiv_pri  = A_units                                                 # all primaries are senior
  A_indiv_sp   = with(df, sum(weight[mfj & pri_senior & sp_senior]))      # spouse senior subset
  A_indiv      = A_indiv_pri + A_indiv_sp

  # Mass B: MFJ + primary < 65 + spouse senior. Spouse contributes 1 senior individual.
  B_units      = with(df, sum(weight[mfj & !pri_senior & sp_senior]))
  B_indiv      = B_units

  # Mass C: non-MFJ with primary senior. Primary contributes 1 senior individual.
  C_units      = with(df, sum(weight[!mfj & pri_senior]))
  C_indiv      = C_units

  cat(sprintf('Total tax units (non-dep): %.2fM\n', sum(df$weight) / 1e6))
  cat(sprintf('Total senior individuals : %.2fM   (= primaries-65+ + spouses-65+)\n',
              total_seniors_indiv / 1e6))
  cat(sprintf('Total senior tax units   : %.2fM   (older >= 65)\n',
              total_senior_taxunits / 1e6))

  cat('\n--- Decomposition (tax units) ---\n')
  cat(sprintf('  Mass A  MFJ, primary >= 65        : %7.2fM\n', A_units / 1e6))
  cat(sprintf('  Mass B  MFJ, primary <65, spouse>=65: %7.2fM\n', B_units / 1e6))
  cat(sprintf('  Mass C  non-MFJ, primary >= 65     : %7.2fM\n', C_units / 1e6))
  cat(sprintf('  --- senior tax units total         : %7.2fM (sum A+B+C)\n',
              (A_units + B_units + C_units) / 1e6))

  cat('\n--- Decomposition (senior individuals) ---\n')
  cat(sprintf('  Mass A primaries (in MFJ band 6)   : %7.2fM\n', A_indiv_pri / 1e6))
  cat(sprintf('  Mass A spouses senior (within A)   : %7.2fM\n', A_indiv_sp / 1e6))
  cat(sprintf('  Mass B spouses senior (primary<65) : %7.2fM\n', B_indiv / 1e6))
  cat(sprintf('  Mass C single-filer seniors        : %7.2fM\n', C_indiv / 1e6))
  cat(sprintf('  --- senior individuals total       : %7.2fM\n',
              (A_indiv + B_indiv + C_indiv) / 1e6))
  cat(sprintf('  sanity: matches direct sum?         : %s\n',
              isTRUE(all.equal(A_indiv + B_indiv + C_indiv, total_seniors_indiv))))

  # Conditional probabilities driving each mass
  cat('\n--- Imputation diagnostics ---\n')
  p_sp_sen_band6 = with(df,
    sum(weight[mfj & age_group == 6L & sp_senior]) /
    sum(weight[mfj & age_group == 6L])
  )
  cat(sprintf('  P(spouse senior | MFJ, primary band 6)         : %.3f\n',
              p_sp_sen_band6))
  for (b in 1:5) {
    p_sp_sen_b = with(df,
      sum(weight[mfj & age_group == b & sp_senior]) /
      sum(weight[mfj & age_group == b])
    )
    n_b = with(df, sum(weight[mfj & age_group == b]))
    cat(sprintf('  P(spouse senior | MFJ, primary band %d) [n=%6.2fM]: %.3f\n',
                b, n_b / 1e6, p_sp_sen_b))
  }

  invisible(list(A = A_units, B = B_units, C = C_units,
                 A_indiv_pri = A_indiv_pri, A_indiv_sp = A_indiv_sp,
                 B_indiv = B_indiv, C_indiv = C_indiv,
                 total_indiv = total_seniors_indiv,
                 mfj_band6 = with(df, sum(weight[mfj & age_group == 6L])),
                 mfj_band6_filers = with(df,
                   sum(weight[mfj & age_group == 6L & is_filer])),
                 mfj_band6_nonfilers = with(df,
                   sum(weight[mfj & age_group == 6L & !is_filer]))))
}

#--------------- Run on 2022 (the file in the user's table) -----------
res22 = decompose(PUF_2022, 'PUF 2022 (projected)')

#--------------- Run on 2017 base year (no projection) ----------------
res17 = decompose(PUF_2017, 'PUF 2017 (base year)')

#--------------- SOI Table 1.6 cross-check (2017) ---------------------

cat('\n\n=== SOI Table 1.6 cross-check (2017) ===\n')
soi = read_csv(SOI_TBL, show_col_types = FALSE)

# Layout: each row is (year, filing_status, age_group, then AGI bin counts).
# Sum across AGI bins to get a total count for that (year, fs, ag) cell.
soi_long = soi %>%
  filter(year == 2017) %>%
  select(-year) %>%
  rowwise() %>%
  mutate(total = sum(c_across(-c(filing_status, age_group)), na.rm = TRUE)) %>%
  ungroup() %>%
  select(filing_status, age_group, total)

soi_mfj_b6  = soi_long %>% filter(filing_status == 2, age_group == 6) %>% pull(total)
soi_mfj_all = soi_long %>% filter(filing_status == 2) %>% pull(total) %>% sum()

cat(sprintf('  SOI 2017: MFJ returns total                 : %7.2fM\n',
            soi_mfj_all / 1e6))
cat(sprintf('  SOI 2017: MFJ + age_group 6 (primary 65+)   : %7.2fM\n',
            soi_mfj_b6 / 1e6))
cat(sprintf('  PUF 2017: MFJ + age_group 6 (all)           : %7.2fM\n',
            res17$mfj_band6 / 1e6))
cat(sprintf('  PUF 2017: MFJ + age_group 6 (filers only)   : %7.2fM\n',
            res17$mfj_band6_filers / 1e6))
cat(sprintf('  PUF 2017: MFJ + age_group 6 (non-filers)    : %7.2fM\n',
            res17$mfj_band6_nonfilers / 1e6))
cat(sprintf('  ratio PUF filers 2017 / SOI 2017            : %.3fx\n',
            res17$mfj_band6_filers / soi_mfj_b6))

# 2022 doesn't have a direct SOI counterpart in this compile; report the
# growth factor implicit in the projection.
cat(sprintf('\n  PUF 2022 / PUF 2017 (MFJ band 6) growth     : %.3fx\n',
            res22$mfj_band6 / res17$mfj_band6))

cat('\nDone.\n')
