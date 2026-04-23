#---------------------------------------------
# bottom_quintile_composition.R
#
# Diagnose why pre-tilt PUF p0-20 by income
# holds 25% of NW while SCF p0-20 holds 1.7%.
# Compares age, income composition, and the
# distribution of pre-tilt wealth within p0-20.
#
# Tests three hypotheses:
#   (1) compositional mismatch (different ages /
#       income sources in p0-20)
#   (2) forest insufficiently discriminating on
#       overall income
#   (3) heavy-tail leaf contamination (few
#       wealthy donors pulling means)
#
# Usage:
#   Rscript src/eda/bottom_quintile_composition.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: Rscript .../bottom_quintile_composition.R <output_dir>')
output_dir = args[1]

cat('Loading SCF, PUF, pre-tilt wealth...\n')
scf      = read_rds('resources/cache/scf_tax_units.rds')
puf_full = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                    show_col_types = FALSE)
puf_pre  = read_rds(file.path(output_dir, 'wealth_pre_tilt.rds'))


#--- SCF prep --------------------------------------------------------------

if (!all(wealth_asset_vars %in% names(scf))) {
  scf = scf %>% mutate(
    cash = LIQ + CDS, equities = STOCKS + STMUTF + COMUTF,
    bonds = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    retirement = IRAKH + THRIFT + FUTPEN + CURRPEN,
    life_ins = CASHLI, annuities = ANNUIT, trusts = TRUSTS,
    other_fin = OTHFIN + OMUTF, pass_throughs = BUS,
    primary_home = HOUSES, other_home = ORESRE, re_fund = NNRESRE,
    other_nonfin = VEHIC + OTHNFIN, primary_mortgage = MRTHEL,
    other_mortgage = RESDBT, credit_lines = OTHLOC,
    credit_cards = CCBAL, installment_debt = INSTALL,
    other_debt = ODEBT
  )
}
scf_age2 = ifelse(is.na(scf$age2), 0L, scf$age2)
scf$age_older = pmax(pmin(80L, scf$age1), pmin(80L, scf_age2))
scf$income = with(scf, wages_scf + business_scf + int_div_scf +
                  capital_gains_scf + rent_scf + ss_pens_scf + ui_other_scf)
scf = cbind(scf, compute_category_values(scf))
scf$pctile = compute_percentile(scf$income, scf$weight)

#--- PUF prep (pre-tilt wealth) --------------------------------------------

puf_age2 = ifelse(is.na(puf_full$age2), 0L, puf_full$age2)
puf_full$age_older = pmax(pmin(80L, puf_full$age1), pmin(80L, puf_age2))
puf_full$income = with(puf_full,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st +
  gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)

# Bring in PRE-TILT wealth (Stage 2 only; no calibration).
puf = puf_full %>% select(-any_of(wealth_y_vars)) %>%
  inner_join(puf_pre, by = 'id') %>%
  bind_cols(compute_category_values(.))
puf$pctile = compute_percentile(puf$income, puf$weight)


#--- Slice bottom quintile -------------------------------------------------

scf_q1 = scf %>% filter(pctile < 20)
puf_q1 = puf %>% filter(pctile < 20)

wsum = function(x, w) sum(x * w)
wmean = function(x, w) sum(x * w) / sum(w)
wqtile = function(x, w, p) {
  o = order(x); x = x[o]; w = w[o]
  cp = cumsum(w) / sum(w)
  approx(cp, x, xout = p, rule = 2)$y
}


#--- Hypothesis 1 — compositional stats -----------------------------------

cat('\n================================================================\n')
cat('  BOTTOM QUINTILE (p0-20 by weighted income, unfloored)  \n')
cat('================================================================\n\n')

cat(sprintf(
'counts
  SCF:  unwtd n = %d      weighted pop = %.1fM
  PUF:  unwtd n = %d      weighted pop = %.1fM

',
  nrow(scf_q1), sum(scf_q1$weight) / 1e6,
  nrow(puf_q1), sum(puf_q1$weight) / 1e6))

demog_row = function(label, fmt, sval, pval) {
  cat(sprintf('  %-28s  SCF %s   PUF %s\n',
              label, sprintf(fmt, sval), sprintf(fmt, pval)))
}

cat('AGE\n')
demog_row('mean age_older',       '%6.1f', wmean(scf_q1$age_older, scf_q1$weight),
                                            wmean(puf_q1$age_older, puf_q1$weight))
demog_row('median age_older',     '%6.1f', wqtile(scf_q1$age_older, scf_q1$weight, 0.5),
                                            wqtile(puf_q1$age_older, puf_q1$weight, 0.5))
demog_row('frac age_older >= 65', '%6.1f%%',
          100*wmean(scf_q1$age_older >= 65, scf_q1$weight),
          100*wmean(puf_q1$age_older >= 65, puf_q1$weight))
demog_row('frac age_older < 40',  '%6.1f%%',
          100*wmean(scf_q1$age_older < 40, scf_q1$weight),
          100*wmean(puf_q1$age_older < 40, puf_q1$weight))
demog_row('frac age_older 40-64', '%6.1f%%',
          100*wmean(scf_q1$age_older >= 40 & scf_q1$age_older < 65, scf_q1$weight),
          100*wmean(puf_q1$age_older >= 40 & puf_q1$age_older < 65, puf_q1$weight))

cat('\nINCOME\n')
demog_row('mean income',        '$%10.0f', wmean(scf_q1$income, scf_q1$weight),
                                             wmean(puf_q1$income, puf_q1$weight))
demog_row('median income',      '$%10.0f', wqtile(scf_q1$income, scf_q1$weight, 0.5),
                                             wqtile(puf_q1$income, puf_q1$weight, 0.5))
demog_row('frac income <= 0',   '%6.1f%%',
          100*wmean(scf_q1$income <= 0, scf_q1$weight),
          100*wmean(puf_q1$income <= 0, puf_q1$weight))
demog_row('frac income < 0',    '%6.1f%%',
          100*wmean(scf_q1$income  < 0, scf_q1$weight),
          100*wmean(puf_q1$income  < 0, puf_q1$weight))

cat('\nINCOME SOURCES — frac with positive value\n')
demog_row('any wages',      '%6.1f%%',
          100*wmean(scf_q1$wages_scf > 0, scf_q1$weight),
          100*wmean(puf_q1$wages > 0, puf_q1$weight))
demog_row('any business',   '%6.1f%%',
          100*wmean(scf_q1$business_scf > 0, scf_q1$weight),
          100*wmean((puf_q1$sole_prop + puf_q1$farm +
                     puf_q1$scorp_active + puf_q1$scorp_passive +
                     puf_q1$part_active  + puf_q1$part_passive) > 0, puf_q1$weight))
demog_row('any int_div',    '%6.1f%%',
          100*wmean(scf_q1$int_div_scf > 0, scf_q1$weight),
          100*wmean((puf_q1$txbl_int + puf_q1$exempt_int +
                     puf_q1$div_ord + puf_q1$div_pref) > 0, puf_q1$weight))
demog_row('any capital gains','%6.1f%%',
          100*wmean(scf_q1$capital_gains_scf > 0, scf_q1$weight),
          100*wmean((puf_q1$kg_lt + puf_q1$kg_st) > 0, puf_q1$weight))
demog_row('any SS or pens', '%6.1f%%',
          100*wmean(scf_q1$ss_pens_scf > 0, scf_q1$weight),
          100*wmean((puf_q1$gross_ss + puf_q1$gross_pens_dist) > 0, puf_q1$weight))

cat('\nINCOME SOURCES — mean $ among positives\n')
mpos = function(x, w) {
  if (!any(x > 0)) return(0)
  wmean(x[x > 0], w[x > 0])
}
demog_row('wages | wages>0', '$%10.0f',
          mpos(scf_q1$wages_scf, scf_q1$weight),
          mpos(puf_q1$wages, puf_q1$weight))
demog_row('SS/pens | SS/pens>0', '$%10.0f',
          mpos(scf_q1$ss_pens_scf, scf_q1$weight),
          mpos(puf_q1$gross_ss + puf_q1$gross_pens_dist, puf_q1$weight))
demog_row('int_div | >0', '$%10.0f',
          mpos(scf_q1$int_div_scf, scf_q1$weight),
          mpos(puf_q1$txbl_int + puf_q1$exempt_int +
               puf_q1$div_ord + puf_q1$div_pref, puf_q1$weight))


#--- Hypothesis 3 — pre-tilt wealth distribution --------------------------

cat('\n\nPRE-TILT NET WORTH DISTRIBUTION WITHIN p0-20\n')
demog_row('mean NW',       '$%10.0f', wmean(scf_q1$cat_nw, scf_q1$weight),
                                       wmean(puf_q1$cat_nw, puf_q1$weight))
demog_row('median NW',     '$%10.0f', wqtile(scf_q1$cat_nw, scf_q1$weight, 0.5),
                                       wqtile(puf_q1$cat_nw, puf_q1$weight, 0.5))
demog_row('p90 NW',        '$%10.0f', wqtile(scf_q1$cat_nw, scf_q1$weight, 0.9),
                                       wqtile(puf_q1$cat_nw, puf_q1$weight, 0.9))
demog_row('p99 NW',        '$%10.0f', wqtile(scf_q1$cat_nw, scf_q1$weight, 0.99),
                                       wqtile(puf_q1$cat_nw, puf_q1$weight, 0.99))
demog_row('max NW',        '$%10.0f', max(scf_q1$cat_nw), max(puf_q1$cat_nw))
demog_row('frac NW > $1M', '%6.1f%%',
          100*wmean(scf_q1$cat_nw > 1e6, scf_q1$weight),
          100*wmean(puf_q1$cat_nw > 1e6, puf_q1$weight))
demog_row('frac NW > $10M','%6.2f%%',
          100*wmean(scf_q1$cat_nw > 1e7, scf_q1$weight),
          100*wmean(puf_q1$cat_nw > 1e7, puf_q1$weight))


#--- Age × wealth cross-tab within p0-20 ----------------------------------

cat('\n\nWITHIN p0-20: age × mean(NW)  (who has the money in the bottom quintile)\n')
age_bands = function(df, lab) {
  ab = cut(df$age_older, breaks = c(0, 40, 55, 65, 80),
           include.lowest = TRUE, right = FALSE,
           labels = c('<40','40-54','55-64','65+'))
  df %>% mutate(band = ab) %>%
    group_by(band) %>%
    summarise(
      n_unwt   = dplyr::n(),
      n_wt_m   = sum(weight) / 1e6,
      mean_nw  = sum(weight * cat_nw) / sum(weight),
      total_nw_t = sum(weight * cat_nw) / 1e12,
      share_of_q1_nw = NA_real_,
      .groups = 'drop'
    ) %>%
    mutate(share_of_q1_nw = total_nw_t / sum(total_nw_t),
           source = lab)
}
scf_ab = age_bands(scf_q1, 'SCF')
puf_ab = age_bands(puf_q1, 'PUF')
cat('\nSCF p0-20:\n')
print(scf_ab %>% mutate(n_wt_m = round(n_wt_m, 1),
                         mean_nw = round(mean_nw),
                         total_nw_t = round(total_nw_t, 2),
                         share_of_q1_nw = round(share_of_q1_nw, 3)),
      n = Inf)
cat('\nPUF p0-20:\n')
print(puf_ab %>% mutate(n_wt_m = round(n_wt_m, 1),
                         mean_nw = round(mean_nw),
                         total_nw_t = round(total_nw_t, 2),
                         share_of_q1_nw = round(share_of_q1_nw, 3)),
      n = Inf)
