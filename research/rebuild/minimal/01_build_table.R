#--------------------------------------
# 01_build_table.R  (minimal example, step 1, part 1 of 2)
#
# What it does: builds one stacked tax-unit table from the 2015 PUF and the
# 2016 SCF (tax year 2015) with seven variables: married, n_dep, wages,
# interest (taxable only), dividends, capital_gains, other_agi (PUF only),
# net_worth (SCF only). Amounts are in
# 2015 dollars. No filing rule. Dropped: the PUF aggregate records, and
# the SCF split-off units (2026-10-14; see the comment at the drop).
#   PUF: read as src/process_puf.R does in its first 25 lines.
#   SCF: production builder build_scf_tax_units() (src/imputations/
#     stage1_scf_tax_units.R), fed a copy of the SCFP file in which four
#     income columns are replaced by the raw interview items in 2015 dollars,
#     so the builder's own split rules apply to them (see "SCF" below).
# What it writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/):
#   raw_stacked.csv.gz   one row per PUF record or SCF unit x implicate
#   checks_r.csv         dollar factor, identity checks, NA counts, counts
#   scfp2016_rawincome.csv  the modified SCFP copy handed to the builder
# What to look at: checks_r.csv. The "identity" rows should be ~0, the
# dollar factor should be constant across records (min = max).
# Run from the repo root through Slurm: sbatch research/rebuild/minimal/01_run.sbatch
#
# Wave mode (added 2026-10-14 for the several-years table):
#   Rscript research/rebuild/minimal/01_build_table.R --scf-wave 2022
# builds one SCF wave only, the same way (no PUF), with year = wave - 1 (the
# tax year of the incomes) and net worth divided by the wave's ASSET factor,
# so it is in the interview year's dollars (the step-1 build above uses the
# income factor instead, putting 2016 wealth in 2015 prices). Writes
#   scf<wave>_units.csv.gz, checks_scf<wave>.csv, wave<wave>_scfp_rawincome.csv
# and leaves the step-1 files alone. Run by 07_run_years.sbatch.
#--------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(data.table); library(haven); library(yaml)
})

OUT  = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data'
args = commandArgs(trailingOnly = TRUE)
STEP1 = length(args) == 0              # step-1 build: PUF + SCF 2016
if (!STEP1) stopifnot(length(args) == 2, args[1] == '--scf-wave')
WAVE = if (STEP1) 2016L else as.integer(args[2])
YEAR = WAVE - 1L   # tax year of the SCF incomes (2015 for the PUF too)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

chk = list()
add = function(group, key, value) {
  chk[[length(chk) + 1]] <<- tibble(group = group, key = key, value = as.numeric(value))
}

# Interface paths, built the way src/configure.R builds interface_paths.
# configure.R itself is not sourced: with write_locally = False it creates a
# vintage directory and writes dependencies.csv under the shared production
# root, which this experiment must not touch.
output_roots = read_yaml('config/interfaces/output_roots.yaml')
iv = read_yaml('config/interfaces/interface_versions.yaml')
rs = read_yaml('config/runscripts/baseline.yaml')
iface = function(nm) file.path(output_roots$production, iv[[nm]]$type, nm,
                               paste0('v', iv[[nm]]$version),
                               rs$dependency_info[[nm]]$vintage,
                               rs$dependency_info[[nm]]$scenario)
puf_dir = iface('IRS-PUF')
scf_dir = file.path(output_roots$production, iv$SCF$type, 'SCF',
                    paste0('v', iv$SCF$version), WAVE, 'historical')

if (STEP1) {

#------------------------------------------------------------------
# PUF (src/process_puf.R lines 10-21)
#------------------------------------------------------------------

raw_puf = puf_dir %>%
  file.path('puf_2015.csv') %>%
  read_csv(show_col_types = FALSE) %>%
  left_join(puf_dir %>% file.path('demographics_2015.csv') %>%
              read_csv(show_col_types = FALSE),
            by = 'RECID')
puf = raw_puf %>% filter(RECID < 999996)   # drop the four aggregate records
add('puf', 'records_in_file', nrow(raw_puf))
add('puf', 'records_kept', nrow(puf))
stopifnot(!anyDuplicated(puf$RECID))

need = c('RECID', 'S006', 'MARS', 'XOCAH', 'XOCAWH', 'XOPAR', 'XOODEP',
         'E00100', 'E00200', 'E00300', 'E00400', 'E00600', 'E00650', 'E01000', 'DSI')
stopifnot(all(need %in% names(puf)))
for (v in need) add('puf_na_count', v, sum(is.na(puf[[v]])))
if (any(is.na(puf[need]))) stop('PUF: NA in a needed column; see checks_r.csv')

n_dep_puf = puf$XOCAH + puf$XOCAWH + puf$XOPAR + puf$XOODEP
add('puf', 'n_dep_sum_negative_records', sum(n_dep_puf < 0))
add('puf', 'n_dep_sum_above_3_records', sum(n_dep_puf > 3))
add('puf', 'E00650_qualified_gt_E00600_ordinary_records', sum(puf$E00650 > puf$E00600))
add('puf', 'E00200_negative_records', sum(puf$E00200 < 0))
stopifnot(all(n_dep_puf >= 0))

puf_tab = puf %>%
  transmute(source = 'puf', year = YEAR,
            record_id = as.numeric(RECID), household_id = as.numeric(RECID),
            implicate = 0L, weight = S006 / 100,
            married = as.integer(MARS == 2),
            n_dep = as.integer(pmin(n_dep_puf, 3)),
            # interest: taxable interest only (E00300), 2026-10-14, John's
            # decision; tax-exempt E00400 left out on both sides.
            wages = E00200, interest = E00300, dividends = E00600,
            capital_gains = E01000,
            # other_agi: the rest of AGI (business, pensions, taxable Social
            # Security, unemployment, ... minus adjustments). PUF only.
            other_agi = E00100 - E00200 - E00300 - E00600 - E01000,
            net_worth = NA_real_)

# AGI must be recoverable from the parts on every PUF record, to the cent.
agi_gap = with(puf_tab, wages + interest + dividends + capital_gains + other_agi) - puf$E00100
add('identity_max_abs_dollars', 'puf_agi_parts_vs_E00100', max(abs(agi_gap)))
stopifnot(all(abs(agi_gap) < 0.005))
add('puf', 'other_agi_negative_records', sum(puf_tab$other_agi < 0))

# The earlier test removed dependent filers (DSI = 1); this table keeps them.
# Shares with any amount, without them, for comparison with that test.
nd = puf$DSI == 0
w  = puf$S006 / 100
add('puf', 'dependent_filer_records', sum(!nd))
add('puf', 'dependent_filer_weighted_millions', sum(w[!nd]) / 1e6)
for (v in c('interest', 'dividends', 'capital_gains'))
  add('puf_share_nonzero_excluding_dependent_filers', v,
      sum(w[nd] * (puf_tab[[v]][nd] != 0)) / sum(w[nd]))
}  # end PUF (step-1 build only)

#------------------------------------------------------------------
# SCF wave (2016 in the step-1 build)
#------------------------------------------------------------------

source('src/imputations/wealth_schema.R')

# Load only the builder's two function definitions. Sourcing the whole file
# would also run its cache block, which reads interface_paths and rebuilds
# and overwrites resources/cache/scf_tax_units.rds (the 2022 production cache).
ex = as.list(parse('src/imputations/stage1_scf_tax_units.R', keep.source = FALSE))
is_def = function(e) is.call(e) && as.character(e[[1]]) %in% c('=', '<-') &&
  is.name(e[[2]]) && as.character(e[[2]]) %in% c('default_stage1_config', 'build_scf_tax_units')
defs = Filter(is_def, ex)
stopifnot(length(defs) == 2)
for (e in defs) eval(e, envir = globalenv())

raw_path  = file.path(scf_dir, sprintf('p%02di6.dta', WAVE %% 100))
scfp_path = file.path(scf_dir, sprintf('SCFP%d.csv', WAVE))
stopifnot(file.exists(raw_path), file.exists(scfp_path))

# Raw interview items, in the tax year's dollars (2016 codebook; for other
# waves the file's own variable labels are printed to the log to confirm):
#   X5702 wages and salaries (1040 line 7)
#   X5706 tax-exempt interest (line 8b; read for checks, not used),
#   X5708 other (taxable) interest (line 8a): the interest variable
#   X5710 dividends (lines 9a, 9b)
#   X5712 net gains or losses from mutual funds, stocks, bonds, real estate
#         (lines 13, 14); -1 means "nothing"
#   X6410, X6411 split-off members' interest and dividend flags (Section Y)
#   X3902 savings bonds, X4006 cash value of life insurance (factor checks)
items = c('y1', 'yy1', 'x42001', 'x5702', 'x5706', 'x5708', 'x5710', 'x5712',
          'x6410', 'x6411', 'x3902', 'x4006')
hh = read_dta(raw_path, col_select = all_of(items))
for (v in items) cat(sprintf('label %d %s: %s\n', WAVE, v, paste(attr(hh[[v]], 'label'), collapse = ' ')))
hh = hh %>% mutate(across(everything(), as.numeric))
names(hh) = toupper(names(hh))
stopifnot(!anyDuplicated(hh$Y1), !anyNA(hh))
for (v in c('X5702', 'X5706', 'X5708', 'X5710'))
  add('scf_raw_negative_values', v, sum(hh[[v]] < 0))
# 2022 wave: 20 rows (4 households) have X5702 = -1 with X5701 = 1 ("had
# wages"). The 2022 codebook lists no -1 code for X5702, but -1 means
# "nothing" wherever the SCF does list it (X5704, X5712), so it is set to 0.
# The SCFP's WAGEINC on those rows is recorded to check how the Fed read it.
wage_m1 = hh$Y1[hh$X5702 == -1]
add('scf_raw_codes', 'X5702_eq_-1_set_to_0', length(wage_m1))
hh = hh %>% mutate(X5702 = if_else(X5702 == -1, 0, X5702))
stopifnot(all(hh$X5702 >= 0), all(hh$X5706 >= 0), all(hh$X5708 >= 0), all(hh$X5710 >= 0))
add('scf_raw_codes', 'X5712_eq_-1_nothing', sum(hh$X5712 == -1))
add('scf_raw_codes', 'X5712_other_negative_losses', sum(hh$X5712 < -1))
hh = hh %>% mutate(kg_raw = if_else(X5712 == -1, 0, X5712))

# Dollar factor: SCFP (2022 dollars) over the raw item, record by record.
scfp = fread(scfp_path)
fx = as_tibble(scfp) %>% select(Y1, WGT, WAGEINC, NETWORTH, SAVBND, CASHLI) %>%
  inner_join(hh, by = 'Y1')
stopifnot(nrow(fx) == nrow(scfp), nrow(fx) == nrow(hh))
if (length(wage_m1)) add('scf_raw_codes', 'X5702_eq_-1_SCFP_WAGEINC_max_abs',
                         max(abs(fx$WAGEINC[fx$Y1 %in% wage_m1])))
rw = with(fx[fx$X5702 > 0, ], WAGEINC / X5702)
FACTOR = median(rw)
add('dollar_factor', 'WAGEINC_over_X5702_median_used', FACTOR)
add('dollar_factor', 'WAGEINC_over_X5702_min', min(rw))
add('dollar_factor', 'WAGEINC_over_X5702_max', max(rw))
add('dollar_factor', 'records_with_wages', length(rw))
rs_ = with(fx[fx$X3902 > 0, ], SAVBND / X3902)
add('dollar_factor_assets', 'SAVBND_over_X3902_min', min(rs_))
add('dollar_factor_assets', 'SAVBND_over_X3902_max', max(rs_))
rc_ = with(fx[fx$X4006 > 0, ], CASHLI / X4006)
add('dollar_factor_assets', 'CASHLI_over_X4006_min', min(rc_))
add('dollar_factor_assets', 'CASHLI_over_X4006_max', max(rc_))
ASSET_FACTOR = median(rs_)
add('dollar_factor_assets', 'SAVBND_over_X3902_median', ASSET_FACTOR)
# Each factor must be one constant, and the two asset items must agree.
stopifnot(max(abs(rw / FACTOR - 1)) < 1e-6, max(abs(rs_ / ASSET_FACTOR - 1)) < 1e-6,
          max(abs(rc_ / ASSET_FACTOR - 1)) < 1e-6)
NW_FACTOR = if (STEP1) FACTOR else ASSET_FACTOR
add('dollar_factor', 'factor_used_for_net_worth', NW_FACTOR)
add('scf', 'max_abs_diff_SCFP_WGT_vs_X42001_over_5', max(abs(fx$WGT - fx$X42001 / 5)))

# Hand the builder a copy of the SCFP whose four income columns hold the raw
# items. The builder then splits them exactly as it splits its own: WAGEINC
# by each spouse's wages (share_labor) and INTDIVINC, RENT, KGINC by shares of
# earned income (share_earn), and adds split-off members' Section-Y income.
# RENT is used as a carrier for dividends: for couple and single units it
# comes out as dividends x share_earn; for split-off units it is junk (their
# rent) and is replaced below.
scfp_mod = as_tibble(scfp) %>% select(-WAGEINC, -INTDIVINC, -RENT, -KGINC) %>%
  left_join(hh %>% transmute(Y1, WAGEINC = X5702, INTDIVINC = X5708,
                             RENT = X5710, KGINC = kg_raw), by = 'Y1')
mod_path = file.path(OUT, if (STEP1) 'scfp2016_rawincome.csv' else sprintf('wave%d_scfp_rawincome.csv', WAVE))
fwrite(scfp_mod, mod_path)

tu = build_scf_tax_units(default_stage1_config(), raw_path, mod_path, syear = WAVE)

tu = tu %>% mutate(Y1 = taxunitid %/% 100) %>%
  left_join(hh %>% select(Y1, X6410, X6411), by = 'Y1')
is_npeu = tu$taxunit_code >= 3
f10 = as.numeric(tu$X6410 == 1); f11 = as.numeric(tu$X6411 == 1)
# Split-off units: the builder's int_div_scf is INTINC + DIVINC, each equal to
# the same per-type share of X6415 times its flag; split it back by the flags.
stopifnot(all(abs(tu$int_div_scf[is_npeu & f10 + f11 == 0]) < 1e-9))
nw_2022 = rowSums(as.matrix(tu[, wealth_asset_vars])) - rowSums(as.matrix(tu[, wealth_debt_vars]))

scf_tab = tu %>%
  transmute(source = 'scf', year = YEAR,
            record_id = as.numeric(taxunitid), household_id = as.numeric(scf_hh_id),
            implicate = as.integer(implicate), weight = weight,
            married = as.integer(married == 1 & filestat == 2),
            n_dep = as.integer(pmin(n_dep, 3)),
            wages = wages_scf,
            interest  = if_else(is_npeu, if_else(f10 + f11 > 0, int_div_scf * f10 / (f10 + f11), 0), int_div_scf),
            dividends = if_else(is_npeu, if_else(f10 + f11 > 0, int_div_scf * f11 / (f10 + f11), 0), rent_scf),
            capital_gains = capital_gains_scf,
            other_agi = NA_real_,   # PUF-owned; not observed in the SCF
            # Split-off units: wealth not asked (builder sets zero); unobserved
            # per John's 9 October decision (scf_joint DATA_REPORT section 2).
            net_worth = if_else(is_npeu, NA_real_, nw_2022 / NW_FACTOR))
stopifnot(!anyDuplicated(scf_tab$record_id))

# Checks: couple and single units add up to the household's raw items.
peu = scf_tab[!is_npeu, ] %>% mutate(Y1 = tu$Y1[!is_npeu], nw = nw_2022[!is_npeu]) %>%
  group_by(Y1) %>%
  summarise(across(c(wages, interest, dividends, capital_gains, nw), sum), .groups = 'drop') %>%
  inner_join(fx %>% select(Y1, X5702, X5706, X5708, X5710, kg_raw, NETWORTH), by = 'Y1')
stopifnot(nrow(peu) == nrow(hh))
add('identity_max_abs_dollars', 'wages_units_vs_X5702', max(abs(peu$wages - peu$X5702)))
add('identity_max_abs_dollars', 'interest_units_vs_X5708', max(abs(peu$interest - peu$X5708)))
add('identity_max_abs_dollars', 'dividends_units_vs_X5710', max(abs(peu$dividends - peu$X5710)))
add('identity_max_abs_dollars', 'capital_gains_units_vs_X5712', max(abs(peu$capital_gains - peu$kg_raw)))
add('identity_max_abs_dollars', 'networth_units_vs_SCFP_NETWORTH_2022dollars', max(abs(peu$nw - peu$NETWORTH)))

ws = scf_tab$weight
add('scf', 'households', n_distinct(scf_tab$household_id))
add('scf', 'unit_rows_all_implicates', nrow(scf_tab))
add('scf', 'split_off_unit_rows', sum(is_npeu))
add('scf', 'split_off_weighted_millions', sum(ws[is_npeu]) / 1e6)
add('scf', 'married_split_couple_halves_weighted_millions',
    sum(ws[tu$married == 1 & tu$filestat != 2]) / 1e6)
add('scf', 'n_dep_builder_above_3_rows', sum(tu$n_dep > 3))
for (v in c('wages', 'interest', 'dividends', 'capital_gains'))
  add('scf_split_off_weighted_total_billions', v, sum(ws[is_npeu] * scf_tab[[v]][is_npeu]) / 1e9)

#------------------------------------------------------------------
# Stack, audit NAs, write
#------------------------------------------------------------------

# Drop the SCF split-off units (2026-10-14, John's decision). They are
# financially independent adults living in an SCF household; the SCF asks
# their income but not their wealth, so their net_worth was missing. Dropping
# them leaves net_worth present for every SCF row. Every household keeps its
# couple or single unit, so the set of household ids, and with it the
# household-level split in 01_band.py, is unchanged. The checks above are
# computed before the drop.
n_hh_before = n_distinct(scf_tab$household_id)
scf_tab = scf_tab[!is_npeu, ]
stopifnot(n_distinct(scf_tab$household_id) == n_hh_before)
add('scf', 'unit_rows_after_dropping_split_offs', nrow(scf_tab))
add('scf', 'weighted_millions_after_dropping_split_offs', sum(scf_tab$weight) / 1e6)

if (!STEP1) {
  for (v in names(scf_tab)) add('na_count_scf', v, sum(is.na(scf_tab[[v]])))
  if (scf_tab %>% select(-other_agi) %>% is.na() %>% any()) stop('NA in the SCF wave; see checks')
  fwrite(scf_tab, file.path(OUT, sprintf('scf%d_units.csv.gz', WAVE)))
  chk = bind_rows(chk)
  write_csv(chk, file.path(OUT, sprintf('checks_scf%d.csv', WAVE)))
  print(as.data.frame(chk), digits = 10)
  cat(sprintf('01_build_table: wave %d, wrote %d unit rows\n', WAVE, nrow(scf_tab)))
  quit(save = 'no')
}

stacked = bind_rows(puf_tab, scf_tab)
for (s in c('puf', 'scf')) for (v in names(stacked))
  add(paste0('na_count_', s), v, sum(is.na(stacked[[v]][stacked$source == s])))
other_na = stacked %>% select(-net_worth, -other_agi) %>% is.na() %>% sum()
if (other_na > 0) stop('NA outside net_worth and other_agi; see checks_r.csv')
stopifnot(sum(is.na(stacked$net_worth)) == nrow(puf_tab),
          sum(is.na(stacked$other_agi)) == nrow(scf_tab),
          all(is.na(stacked$other_agi) == (stacked$source == 'scf')))

fwrite(stacked, file.path(OUT, 'raw_stacked.csv.gz'))
chk = bind_rows(chk)
write_csv(chk, file.path(OUT, 'checks_r.csv'))
print(as.data.frame(chk), digits = 10)
cat(sprintf('01_build_table: wrote %d rows (%d PUF, %d SCF)\n',
            nrow(stacked), nrow(puf_tab), nrow(scf_tab)))
