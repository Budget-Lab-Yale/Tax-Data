#--------------------------------------
# 06_build_scf2019.R  (minimal example, step 6, part 1 of 2)
#
# What it does: builds 2019 SCF tax units the way step 1 (01_build_table.R)
# built the 2016 wave: the production builder build_scf_tax_units()
# (src/imputations/stage1_scf_tax_units.R, wealth_schema.R sourced first,
# only the builder's two function definitions loaded, so its cache block
# does not run), fed a copy of the SCFP file whose four income columns hold
# the raw interview items. Split-off units are dropped, as in step 1.
#
# Years and dollars. The 2019 wave asks about income in tax year 2018 and
# measures wealth at the 2019 interview. So:
#   wages, interest, dividends, capital_gains   2018 dollars, as reported
#                                               (raw items X5702, X5708,
#                                               X5710, X5712; same item
#                                               numbers as in 2016)
#   net_worth                  2019 dollars: the SCFP's 2022-dollar net worth
#                              divided by the wave's ASSET factor, verified on
#                              two asset items (SAVBND / X3902 and
#                              CASHLI / X4006). The income factor (WAGEINC /
#                              X5702) is recorded too; it differs because
#                              the incomes are a year earlier.
#   net_worth_2015_dollars     the SCFP's 2022-dollar net worth divided by
#                              1.2493, the factor step 1 used to put the 2016
#                              wave's net worth in 2015 dollars. This puts the
#                              2019 wealth on the same price basis as the
#                              stacked table's net_worth.
#   year = 2018, the tax year of the incomes (step 1 set year = 2015 for the
#   2016 wave); wealth_year = 2019.
#
# Writes, in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/:
#   scf2019_units.csv.gz     one row per couple or single unit x implicate
#                            (06_run_scf2019.sbatch converts it to
#                            scf2019_units.parquet)
#   scfp2019_rawincome.csv   the modified SCFP copy handed to the builder
#   checks_scf2019.csv       dollar factors, identity checks, counts
# Run from the repo root through Slurm: sbatch research/rebuild/minimal/06_run_scf2019.sbatch
#--------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(data.table); library(haven); library(yaml)
})

OUT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data'
FACTOR_2016_WAVE = 1.2493   # step 1: 2022 dollars over 2015 dollars, 2016 wave
chk = list()
add = function(group, key, value) {
  chk[[length(chk) + 1]] <<- tibble(group = group, key = key, value = as.numeric(value))
}

output_roots = read_yaml('config/interfaces/output_roots.yaml')
iv = read_yaml('config/interfaces/interface_versions.yaml')
scf_dir = file.path(output_roots$production, iv$SCF$type, 'SCF',
                    paste0('v', iv$SCF$version), '2019', 'historical')

source('src/imputations/wealth_schema.R')
ex = as.list(parse('src/imputations/stage1_scf_tax_units.R', keep.source = FALSE))
is_def = function(e) is.call(e) && as.character(e[[1]]) %in% c('=', '<-') &&
  is.name(e[[2]]) && as.character(e[[2]]) %in% c('default_stage1_config', 'build_scf_tax_units')
defs = Filter(is_def, ex)
stopifnot(length(defs) == 2)
for (e in defs) eval(e, envir = globalenv())

raw_path  = file.path(scf_dir, 'p19i6.dta')
scfp_path = file.path(scf_dir, 'SCFP2019.csv')
stopifnot(file.exists(raw_path), file.exists(scfp_path))

items = c('y1', 'yy1', 'x42001', 'x5702', 'x5706', 'x5708', 'x5710', 'x5712',
          'x6410', 'x6411', 'x3902', 'x4006')
hh = read_dta(raw_path, col_select = all_of(items)) %>%
  mutate(across(everything(), as.numeric))
names(hh) = toupper(names(hh))
stopifnot(!anyDuplicated(hh$Y1), !anyNA(hh))
for (v in c('X5702', 'X5706', 'X5708', 'X5710'))
  add('scf_raw_negative_values', v, sum(hh[[v]] < 0))
stopifnot(all(hh$X5702 >= 0), all(hh$X5706 >= 0), all(hh$X5708 >= 0), all(hh$X5710 >= 0))
add('scf_raw_codes', 'X5712_eq_-1_nothing', sum(hh$X5712 == -1))
add('scf_raw_codes', 'X5712_other_negative_losses', sum(hh$X5712 < -1))
hh = hh %>% mutate(kg_raw = if_else(X5712 == -1, 0, X5712))

# Dollar factors: SCFP (2022 dollars) over the raw item, record by record.
scfp = fread(scfp_path)
fx = as_tibble(scfp) %>% select(Y1, WGT, WAGEINC, NETWORTH, SAVBND, CASHLI) %>%
  inner_join(hh, by = 'Y1')
stopifnot(nrow(fx) == nrow(scfp), nrow(fx) == nrow(hh))
rw = with(fx[fx$X5702 > 0, ], WAGEINC / X5702)
rs_ = with(fx[fx$X3902 > 0, ], SAVBND / X3902)
rc_ = with(fx[fx$X4006 > 0, ], CASHLI / X4006)
for (r in list(list('WAGEINC_over_X5702', rw), list('SAVBND_over_X3902', rs_),
               list('CASHLI_over_X4006', rc_))) {
  add('dollar_factor', paste0(r[[1]], '_median'), median(r[[2]]))
  add('dollar_factor', paste0(r[[1]], '_min'), min(r[[2]]))
  add('dollar_factor', paste0(r[[1]], '_max'), max(r[[2]]))
  add('dollar_factor', paste0(r[[1]], '_records'), length(r[[2]]))
}
ASSET_FACTOR = median(rs_)
# The two asset items must agree on one factor, and it must be constant.
stopifnot(abs(median(rc_) / ASSET_FACTOR - 1) < 1e-4,
          max(abs(rs_ / ASSET_FACTOR - 1)) < 1e-3, max(abs(rc_ / ASSET_FACTOR - 1)) < 1e-3)
add('dollar_factor', 'asset_factor_used_for_net_worth', ASSET_FACTOR)
add('scf', 'max_abs_diff_SCFP_WGT_vs_X42001_over_5', max(abs(fx$WGT - fx$X42001 / 5)))

scfp_mod = as_tibble(scfp) %>% select(-WAGEINC, -INTDIVINC, -RENT, -KGINC) %>%
  left_join(hh %>% transmute(Y1, WAGEINC = X5702, INTDIVINC = X5708,
                             RENT = X5710, KGINC = kg_raw), by = 'Y1')
mod_path = file.path(OUT, 'scfp2019_rawincome.csv')
fwrite(scfp_mod, mod_path)

tu = build_scf_tax_units(default_stage1_config(), raw_path, mod_path, syear = 2019L)
tu = tu %>% mutate(Y1 = taxunitid %/% 100)
is_npeu = tu$taxunit_code >= 3
nw_2022 = rowSums(as.matrix(tu[, wealth_asset_vars])) - rowSums(as.matrix(tu[, wealth_debt_vars]))

# Couple and single units only (split-off units dropped, as in step 1: the
# SCF does not ask their wealth). Their income columns come straight from
# the builder's split; the split-off flag logic of step 1 is not needed.
units = tu[!is_npeu, ] %>%
  mutate(nw_2022 = nw_2022[!is_npeu]) %>%
  transmute(source = 'scf', year = 2018L, wealth_year = 2019L,
            record_id = as.numeric(taxunitid), household_id = as.numeric(scf_hh_id),
            implicate = as.integer(implicate), weight = weight,
            married = as.integer(married == 1 & filestat == 2),
            n_dep = as.integer(pmin(n_dep, 3)),
            wages = wages_scf, interest = int_div_scf, dividends = rent_scf,
            capital_gains = capital_gains_scf,
            net_worth = nw_2022 / ASSET_FACTOR,
            net_worth_2015_dollars = nw_2022 / FACTOR_2016_WAVE,
            Y1 = Y1)
stopifnot(!anyDuplicated(units$record_id))

# Checks: units add up to the household's raw items and SCFP net worth.
peu = units %>% group_by(Y1) %>%
  summarise(across(c(wages, interest, dividends, capital_gains, net_worth), sum), .groups = 'drop') %>%
  inner_join(fx %>% select(Y1, X5702, X5708, X5710, kg_raw, NETWORTH), by = 'Y1')
stopifnot(nrow(peu) == nrow(hh))
add('identity_max_abs_dollars', 'wages_units_vs_X5702', max(abs(peu$wages - peu$X5702)))
add('identity_max_abs_dollars', 'interest_units_vs_X5708', max(abs(peu$interest - peu$X5708)))
add('identity_max_abs_dollars', 'dividends_units_vs_X5710', max(abs(peu$dividends - peu$X5710)))
add('identity_max_abs_dollars', 'capital_gains_units_vs_X5712', max(abs(peu$capital_gains - peu$kg_raw)))
add('identity_max_abs_dollars', 'networth_units_vs_SCFP_NETWORTH_2022dollars',
    max(abs(peu$net_worth * ASSET_FACTOR - peu$NETWORTH)))
for (v in names(units)) add('na_count', v, sum(is.na(units[[v]])))
if (anyNA(units)) stop('NA in the 2019 units; see checks_scf2019.csv')

w = units$weight
add('scf', 'households', n_distinct(units$household_id))
add('scf', 'unit_rows_all_implicates', nrow(units))
add('scf', 'split_off_unit_rows_dropped', sum(is_npeu))
add('scf', 'split_off_weighted_millions_dropped', sum(tu$weight[is_npeu]) / 1e6)
add('scf', 'weighted_millions', sum(w) / 1e6)
add('scf', 'net_worth_total_trillions_2019_dollars', sum(w * units$net_worth) / 1e12)
add('scf', 'net_worth_total_trillions_2015_dollars', sum(w * units$net_worth_2015_dollars) / 1e12)
add('scf', 'SCFP_NETWORTH_total_trillions_2022_dollars', sum(fx$WGT * fx$NETWORTH) / 1e12)

fwrite(units %>% select(-Y1), file.path(OUT, 'scf2019_units.csv.gz'))
chk = bind_rows(chk)
write_csv(chk, file.path(OUT, 'checks_scf2019.csv'))
print(as.data.frame(chk), digits = 10)
cat(sprintf('06_build_scf2019: wrote %d unit rows\n', nrow(units)))
