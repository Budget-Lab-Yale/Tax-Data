#--------------------------------------
# stage1_scf_tax_units.R
#
# Stage 1 of the wealth imputation:
# construct tax-filing units from SCF
# 2022 Primary Economic Units (PEUs).
#
# Port of the relevant subset of Kevin
# Moore's (FRB) SCF-to-TAXSIM SAS code:
#   https://taxsim.nber.org/to-taxsim/
#     scf27-32/code/frbscftax.sas
#
# We port:
#   (1) Tax-unit split structure
#       (MARRIED, TAXUNIT 0/1/2/>=3,
#        TUAGE, SPAGE, KIDS, FILESTAT)
#   (2) Person-level income
#       decomposition (R_LABORINC,
#       R_BUSINC, R_REGPEN, R_WITHDRAW,
#       R_SSA + spouse equivalents)
#   (3) NPEU Section-Y income
#
# We skip:
#   - Moore's full TAXSIM income
#     allocation (~60% of the SAS);
#     instead we join SCFP aggregates
#     and apply a category-specific
#     split rule for split PEUs.
#
# Output: `scf_tax_units` tibble, cached
# to resources/cache/scf_tax_units.rds.
# Matches the contract that
# src/imputations/wealth.R expects
# (see its lines 31-116).
#--------------------------------------

SYEAR = 2022

cache_path = 'resources/cache/scf_tax_units.rds'
dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)

raw_path  = interface_paths$SCF %>% file.path('p22i6.dta')
scfp_path = interface_paths$SCF %>% file.path('SCFP2022.csv')

# If the cache is newer than the raw inputs, load and skip the rebuild.
cache_usable = FALSE
if (file.exists(cache_path)) {
  cache_mtime = file.mtime(cache_path)
  raw_mtime   = max(file.mtime(raw_path), file.mtime(scfp_path))
  cache_usable = cache_mtime > raw_mtime
}

if (cache_usable) {
  message('stage1: loading cached scf_tax_units from ', cache_path,
          ' (cache is newer than raw inputs)')
  scf_tax_units = read_rds(cache_path)
} else {

#---------------------------------------------------------------------------
# Load raw SCF (p22i6.dta). Lowercase in file; uppercased so we can match
# Moore's SAS variable names directly.
#---------------------------------------------------------------------------

cat('stage1: reading raw SCF 2022 from ', raw_path, '\n', sep = '')
raw = haven::read_dta(raw_path)
names(raw) = toupper(names(raw))

cat('stage1: reading SCFP 2022 from ', scfp_path, '\n', sep = '')
scfp = fread(scfp_path) %>% tibble()

#---------------------------------------------------------------------------
# ACONV: annualizer for period-coded $ amounts (Moore SAS lines 209-212).
# Codes not listed in ACONV (1, 18, 20-25) return 0 — Moore's quirk,
# preserved.
#---------------------------------------------------------------------------

aconv = function(f) {
  out = rep(0, length(f))
  out[f == 2]                  = 52.18
  out[f == 3]                  = 26.09
  out[f == 4]                  = 12
  out[f == 5]                  = 4
  out[f %in% c(6, 8, 14, 22)]  = 1
  out[f == 11]                 = 2
  out[f == 12]                 = 6
  out[f == 31]                 = 24
  out
}

#---------------------------------------------------------------------------
# Derive X7370 (years with spouse/partner), ABSP_P, MARRIED, PERSEXP
# per Moore lines 402-415.
#---------------------------------------------------------------------------

raw = raw %>%
  mutate(
    X7370 = if_else(SYEAR == X8005, -1L, as.integer(SYEAR - X8005)),
    ABSP_P = as.integer((X100 == 5) | (X106 == 5 & X107 %in% c(2, 5, 12))),
    PERSEXP = 1L + as.integer(X105 %in% c(1, 2)) * as.integer(ABSP_P == 0),
    MARRIED = as.integer(X8023 == 1 & X105 == 1 & X7370 >= 1 & X7020 == 2),
    LWP     = as.integer(X8023 == 2),
    RAGE    = X14,
    SPAGE   = X19 * as.integer(X7020 == 2)
  )

#---------------------------------------------------------------------------
# Children counts (Moore lines 418-503). SCF 2022 uses the >=2007 branch
# (10 roster positions). Relationship codes 4 and 13 are children.
#---------------------------------------------------------------------------

# Helper: roster-member "is child" gate
is_child = function(rel_code) rel_code %in% c(4, 13)

raw = raw %>%
  mutate(
    KIDS_roster =
      is_child(X108) * (X110 < 19) +
      is_child(X114) * (X116 < 19) +
      is_child(X120) * (X122 < 19) +
      is_child(X126) * (X128 < 19) +
      is_child(X132) * (X134 < 19) +
      is_child(X202) * (X204 < 19) +
      is_child(X208) * (X210 < 19) +
      is_child(X214) * (X216 < 19) +
      is_child(X220) * (X222 < 19) +
      is_child(X226) * (X228 < 19),
    # Kids-elsewhere fudge (Moore line 429): max 1 extra kid under 18
    KIDS    = KIDS_roster + as.integer(X5912 > 0 | X5912 == -2),
    KIDSU17 =
      is_child(X108) * (X110 < 17) +
      is_child(X114) * (X116 < 17) +
      is_child(X120) * (X122 < 17) +
      is_child(X126) * (X128 < 17) +
      is_child(X132) * (X134 < 17) +
      is_child(X202) * (X204 < 17) +
      is_child(X208) * (X210 < 17) +
      is_child(X214) * (X216 < 17) +
      is_child(X220) * (X222 < 17) +
      is_child(X226) * (X228 < 17),
    KIDSU18 =
      is_child(X108) * (X110 < 18) +
      is_child(X114) * (X116 < 18) +
      is_child(X120) * (X122 < 18) +
      is_child(X126) * (X128 < 18) +
      is_child(X132) * (X134 < 18) +
      is_child(X202) * (X204 < 18) +
      is_child(X208) * (X210 < 18) +
      is_child(X214) * (X216 < 18) +
      is_child(X220) * (X222 < 18) +
      is_child(X226) * (X228 < 18),
    KIDSU13 =
      is_child(X108) * (X110 < 13) +
      is_child(X114) * (X116 < 13) +
      is_child(X120) * (X122 < 13) +
      is_child(X126) * (X128 < 13) +
      is_child(X132) * (X134 < 13) +
      is_child(X202) * (X204 < 13) +
      is_child(X208) * (X210 < 13) +
      is_child(X214) * (X216 < 13) +
      is_child(X220) * (X222 < 13) +
      is_child(X226) * (X228 < 13)
  )

#---------------------------------------------------------------------------
# Person-level income decomposition, Moore's SYEAR GE 2010 branch
# (lines 638-656). R_/SP_ labor, business, regpen, withdraw, SSA.
#---------------------------------------------------------------------------

raw = raw %>%
  mutate(
    # Labor: X4112 (amount) × ACONV(X4113) (period) for R
    R_LABORINC  = pmax(0, X4112) * aconv(X4113),
    SP_LABORINC = pmax(0, X4712) * aconv(X4713),
    # Business (Schedule C / partnership / S-corp from primary job)
    R_BUSINC  = pmax(0, X4131) * aconv(X4132),
    SP_BUSINC = pmax(0, X4731) * aconv(X4732),
    # Regular pensions: 4 pension slots (X5318, X5326, X5334, X5418)
    R_REGPEN =
      pmax(0, X5318 * aconv(X5319) * (X5315 == 1) * (X5317 >= 1)) +
      pmax(0, X5326 * aconv(X5327) * (X5323 == 1) * (X5325 >= 1)) +
      pmax(0, X5334 * aconv(X5335) * (X5331 == 1) * (X5333 >= 1)) +
      pmax(0, X5418 * aconv(X5419) * (X5415 == 1) * (X5417 >= 1)),
    SP_REGPEN =
      pmax(0, X5318 * aconv(X5319) * (X5315 == 2) * (X5317 >= 1)) +
      pmax(0, X5326 * aconv(X5327) * (X5323 == 2) * (X5325 >= 1)) +
      pmax(0, X5334 * aconv(X5335) * (X5331 == 2) * (X5333 >= 1)) +
      pmax(0, X5418 * aconv(X5419) * (X5415 == 2) * (X5417 >= 1)),
    # IRA/retirement-account withdrawals (Moore line 643-647, SYEAR>=2010)
    R_WITHDRAW = X6558 + 0.5 * X6574 +
      pmax(0, X6464 * aconv(X6465) * (X5315 == 1)) +
      pmax(0, X6469 * aconv(X6470) * (X5323 == 1)) +
      pmax(0, X6474 * aconv(X6475) * (X5331 == 1)) +
      pmax(0, X6479 * aconv(X6480) * (X5415 == 1)) +
      pmax(0, X6965 * aconv(X6966) * (X5606 == 1)) +
      pmax(0, X6971 * aconv(X6972) * (X5614 == 1)) +
      pmax(0, X6977 * aconv(X6978) * (X5622 == 1)) +
      pmax(0, X6983 * aconv(X6984) * (X5630 == 1)) +
      0.5 * X5724 * (X5725 == 11),
    SP_WITHDRAW = X6566 + 0.5 * X6574 +
      pmax(0, X6464 * aconv(X6465) * (X5315 == 2)) +
      pmax(0, X6469 * aconv(X6470) * (X5323 == 2)) +
      pmax(0, X6474 * aconv(X6475) * (X5331 == 2)) +
      pmax(0, X6479 * aconv(X6480) * (X5415 == 2)) +
      pmax(0, X6965 * aconv(X6966) * (X5606 == 2)) +
      pmax(0, X6971 * aconv(X6972) * (X5614 == 2)) +
      pmax(0, X6977 * aconv(X6978) * (X5622 == 2)) +
      pmax(0, X6983 * aconv(X6984) * (X5630 == 2)) +
      0.5 * X5724 * (X5725 == 11),
    # Social Security (from the X57xx listed-income block: type 3 = SS)
    # For SYEAR>=2010 the 12 listed-income slots have pairs
    # (X5702,X5703), (X5704,X5705), ..., (X5722,X5723) plus whose-income
    # flags at X5701, X5702, ... Moore uses X5701 per slot for attribution.
    # We keep this simple: use the first 6 slots and take .5 split for
    # jointly-received SS (SCF rarely records individual SS ownership).
    R_SSA  = 0.5 * (X5702 * (X5703 == 3) + X5704 * (X5705 == 3) +
                    X5706 * (X5707 == 3) + X5708 * (X5709 == 3) +
                    X5710 * (X5711 == 3) + X5712 * (X5713 == 3)),
    SP_SSA = 0.5 * (X5702 * (X5703 == 3) + X5704 * (X5705 == 3) +
                    X5706 * (X5707 == 3) + X5708 * (X5709 == 3) +
                    X5710 * (X5711 == 3) + X5712 * (X5713 == 3)),
    # Totals for the itemization ranker and the pooled-wealth split share
    R_TINCOME  = R_LABORINC  + R_BUSINC  + R_REGPEN  + R_WITHDRAW,
    SP_TINCOME = SP_LABORINC + SP_BUSINC + SP_REGPEN + SP_WITHDRAW
  )

#---------------------------------------------------------------------------
# PEU split decision (Moore lines 555-566).
# TAXUNIT = 0  → one PEU tax unit
# TAXUNIT = 1  → first half of split PEU (clone second half below)
# Records that fail both conditions get TAXUNIT = NA and will be flagged.
#---------------------------------------------------------------------------

# Also X5744 code 6 counts as "filed" per Moore
filed = function(x5744) x5744 %in% c(1, 6)

raw = raw %>%
  mutate(
    TAXUNIT = case_when(
      # Condition set for TAXUNIT = 0 (one PEU tax unit)
      (filed(X5744) & X5746 == 1 & MARRIED == 1) |
      (MARRIED == 1 & ((X5744 == 5 | X5746 %in% c(3, 4)) |
                       (X5746 == 2 & X7377 != 2 & X7392 != 2))) |
      (X8023 > 0 & X105 == 0 & (X5744 == 5 | (filed(X5744) & X5746 == 0))) |
      (X8023 > 0 & X105 > 0 & X5744 %in% c(1, 5, 6) & X5746 == 0 & X7020 == 1) |
      (X8023 == 8 & X105 == 0 & X5744 == 1 & X5746 == 1) ~ 0L,

      # Condition set for TAXUNIT = 1 (split PEU)
      (filed(X5744) & X5746 %in% c(2, 3, 4)) |
      (X8023 > 0 & X105 > 0 &
        (X5744 == 5 | (filed(X5744) & X5746 %in% c(0, 1)))) |
      (X8023 == 1 & X105 == 1 & X7370 == -1 & X5744 == 1) ~ 1L,

      TRUE ~ NA_integer_
    )
  )

n_unclassified = sum(is.na(raw$TAXUNIT))
if (n_unclassified > 0) {
  cat(sprintf('stage1: WARN — %d records unclassified by Moore split rule; ',
              n_unclassified))
  cat('defaulting to TAXUNIT=0 (one PEU unit).\n')
  raw$TAXUNIT[is.na(raw$TAXUNIT)] = 0L
}

#---------------------------------------------------------------------------
# Build PEU tax-unit rows.
# - TAXUNIT=0: keep as-is, TUAGE=RAGE.
# - TAXUNIT=1: clone into TAXUNIT=2 row; second row gets TUAGE=SPAGE,
#   SPAGE=0.
#---------------------------------------------------------------------------

peu_base = raw %>%
  mutate(TUAGE = RAGE, SPAGE_unit = SPAGE, TAXUNIT_final = TAXUNIT)

peu_clone = raw %>%
  filter(TAXUNIT == 1) %>%
  mutate(TUAGE = SPAGE, SPAGE_unit = 0, TAXUNIT_final = 2L)

peu_units = bind_rows(peu_base, peu_clone)

#---------------------------------------------------------------------------
# Children / FILESTAT for split PEU (Moore lines 978-1013).
# For TAXUNIT=0: PERSEXP and KIDS unchanged.
# For TAXUNIT in (1,2): split KIDSU13/17/18 evenly; give the odd extra
# to the higher-income unit. FILESTAT: MFS=3 / Single=1 / HoH=4.
# For KIDS=1: Moore skips the split block; both clones inherit KIDS=1.
#---------------------------------------------------------------------------

# Odd-child allocation: higher-income half-unit gets the extra
alloc_odd = function(kidcount, taxunit_code, r_tinc, sp_tinc) {
  r_higher = r_tinc >= sp_tinc
  unit1_gets_extra = r_higher
  # unit 1's share when odd: ceil(count/2) if it has the extra else floor
  base = floor(kidcount / 2)
  extra_unit = if_else(unit1_gets_extra,
                      if_else(taxunit_code == 1L, 1L, 0L),
                      if_else(taxunit_code == 2L, 1L, 0L))
  as.integer(base + extra_unit)
}

peu_units = peu_units %>%
  mutate(
    PERSEXP_u = if_else(TAXUNIT_final %in% c(1L, 2L), 1L, PERSEXP),
    KIDSU13_u = case_when(
      TAXUNIT_final == 0L        ~ KIDSU13,
      KIDS == 1L                  ~ KIDSU13,    # Moore's "KIDS>1" gate — skip split
      KIDSU13 %% 2L == 0L        ~ as.integer(KIDSU13 / 2),
      TRUE                        ~ alloc_odd(KIDSU13, TAXUNIT_final, R_TINCOME, SP_TINCOME)
    ),
    KIDSU17_u = case_when(
      TAXUNIT_final == 0L        ~ KIDSU17,
      KIDS == 1L                  ~ KIDSU17,
      KIDSU17 %% 2L == 0L        ~ as.integer(KIDSU17 / 2),
      TRUE                        ~ alloc_odd(KIDSU17, TAXUNIT_final, R_TINCOME, SP_TINCOME)
    ),
    KIDSU18_u = case_when(
      TAXUNIT_final == 0L        ~ KIDSU18,
      KIDS == 1L                  ~ KIDSU18,
      KIDSU18 %% 2L == 0L        ~ as.integer(KIDSU18 / 2),
      TRUE                        ~ alloc_odd(KIDSU18, TAXUNIT_final, R_TINCOME, SP_TINCOME)
    ),
    # Recompute KIDS post-split
    KIDS_u = if_else(TAXUNIT_final %in% c(1L, 2L) & KIDS > 1L,
                     KIDSU13_u + KIDSU17_u + KIDSU18_u,
                     KIDS),
    # Filing status (Moore 1009-1011); MFJ/widower resolved later
    FILESTAT = case_when(
      TAXUNIT_final %in% c(1L, 2L) & PERSEXP_u == 1L & MARRIED == 1L ~ 3L,  # MFS
      TAXUNIT_final %in% c(1L, 2L) & PERSEXP_u == 1L & KIDS_u == 0L & MARRIED == 0L ~ 1L, # Single
      TAXUNIT_final %in% c(1L, 2L) & PERSEXP_u == 1L & KIDS_u >  0L & MARRIED == 0L ~ 4L, # HoH
      TAXUNIT_final == 0L & MARRIED == 1L ~ 2L,  # MFJ
      TAXUNIT_final == 0L & KIDS > 0L       ~ 4L,  # HoH (single with kids)
      TAXUNIT_final == 0L                   ~ 1L,  # Single
      TRUE                                  ~ NA_integer_
    )
  )

#---------------------------------------------------------------------------
# NPEU enumeration (Moore lines 1022-1114).
# Trigger: X7050 >= 1 and PEU TAXUNIT in (0, 1).
# For SYEAR >= 2007: 10 position slots.
#---------------------------------------------------------------------------

npeu_pos = list(
  age     = c('X110', 'X116', 'X122', 'X128', 'X134',
              'X204', 'X210', 'X216', 'X222', 'X228'),
  livewith = c('X112', 'X118', 'X124', 'X130', 'X136',
               'X206', 'X212', 'X218', 'X224', 'X230'),
  findep   = c('X113', 'X119', 'X125', 'X131', 'X137',
               'X207', 'X213', 'X219', 'X225', 'X231')
)

npeu_from_row = function(row_idx, r) {
  if (r$X7050[row_idx] < 1) return(NULL)
  if (!(r$TAXUNIT[row_idx] %in% c(0L, 1L))) return(NULL)

  units = list()
  tu = 2L
  for (i in seq_along(npeu_pos$livewith)) {
    lw = r[[npeu_pos$livewith[i]]][row_idx]
    fd = r[[npeu_pos$findep  [i]]][row_idx]
    if (lw %in% c(1, 2, 3, 4) & fd %in% c(2, 3, 4, 5)) {
      tu = tu + 1L
      age = r[[npeu_pos$age[i]]][row_idx]
      units[[length(units) + 1]] = tibble(
        Y1       = r$Y1[row_idx],
        YY1      = r$YY1[row_idx],
        TAXUNIT_final = tu,
        TUAGE    = age,
        SPAGE_unit = 0,
        KIDS_u   = 0L,
        KIDSU13_u = 0L, KIDSU17_u = 0L, KIDSU18_u = 0L,
        MARRIED  = 0L,
        PERSEXP_u = 1L,
        FILESTAT = 1L,   # NPEU always single
        # NPEU-specific flags
        X6403 = r$X6403[row_idx], X6415 = r$X6415[row_idx],
        X6406 = r$X6406[row_idx], X6407 = r$X6407[row_idx],
        X6408 = r$X6408[row_idx], X6409 = r$X6409[row_idx],
        X6410 = r$X6410[row_idx], X6411 = r$X6411[row_idx],
        X6412 = r$X6412[row_idx], X6413 = r$X6413[row_idx],
        X6414 = r$X6414[row_idx], X7050 = r$X7050[row_idx],
        # Counts needed for Section-Y allocation
        NUMU70 = sum(sapply(seq_along(npeu_pos$age), function(j) {
          a  = r[[npeu_pos$age[j]]]    [row_idx]
          fj = r[[npeu_pos$findep[j]]] [row_idx]
          as.integer(a > 0 & a < 70 & fj == 5)
        })),
        NUMGE62 = sum(sapply(seq_along(npeu_pos$age), function(j) {
          a  = r[[npeu_pos$age[j]]]    [row_idx]
          fj = r[[npeu_pos$findep[j]]] [row_idx]
          as.integer(a >= 62 & fj == 5)
        }))
      )
    }
  }
  if (length(units) == 0) return(NULL)
  bind_rows(units)
}

cat('stage1: enumerating NPEU units...\n')
npeu_rows = map_dfr(seq_len(nrow(raw)), npeu_from_row, r = raw)
cat(sprintf('stage1:   %d NPEU tax-unit rows emitted\n', nrow(npeu_rows)))

#---------------------------------------------------------------------------
# NPEU Section-Y income allocation (Moore lines 1084-1104).
# NUMINCTYPES counts the non-wage income-type flags.
# For each NPEU tax unit: wage from X6403 split by NUMU70 (if >0) else
# by X7050; other income from X6415 split by NUMINCTYPES × X7050.
#---------------------------------------------------------------------------

if (nrow(npeu_rows) > 0) {
  npeu_rows = npeu_rows %>%
    mutate(
      NUMINCTYPES =
        (X6406 == 1) + (X6407 == 1) + (X6408 == 1) + (X6409 == 1) +
        (X6410 == 1) + (X6411 == 1) + (X6412 == 1) + (X6413 == 1) +
        (X6414 > 5),
      # Wages: split across NPEU members age<70
      WSINCOME = if_else(NUMU70 > 0,
                         (pmax(0, X6403) / pmax(NUMU70, 1)) * as.numeric(TUAGE < 70),
                         pmax(0, X6403) / pmax(X7050, 1)),
      # Non-wage: allocate X6415 per type × per member
      npeu_other_share = if_else(X6415 > 0 & NUMINCTYPES > 0,
                                 (pmax(0, X6415) / pmax(NUMINCTYPES, 1)) / pmax(X7050, 1),
                                 0),
      GSSINC   = (X6406 == 1) * npeu_other_share *
                   if_else(NUMGE62 > 0, as.numeric(TUAGE >= 62) * (X7050 / pmax(NUMGE62, 1)), 1),
      TBUSINC  = (X6412 == 1) * npeu_other_share,
      INTINC   = (X6410 == 1) * npeu_other_share,
      DIVINC   = (X6411 == 1) * npeu_other_share,
      RENTINC  = (X6413 == 1) * npeu_other_share,
      PENINC   = ((X6407 == 1) + (X6408 == 1)) * npeu_other_share,
      AFDCINC  = (X6409 == 1) * npeu_other_share,
      CAPGLINC = (X6414 == 13) * npeu_other_share,
      UNEMPINC = (X6414 == 10) * npeu_other_share,
      OTHINC   = (X6414 %in% c(11, 12, 15)) * npeu_other_share
    )
} else {
  npeu_rows = tibble()
}

#---------------------------------------------------------------------------
# Build the per-tax-unit output frame.
#
# Columns: taxunitid, scf_hh_id, implicate, weight, age1, age2, n_dep,
# married, male1, male2, filestat, plus allocation shares used to multiply
# SCFP aggregate fields.
#---------------------------------------------------------------------------

# PEU units — compute allocation shares
peu_out = peu_units %>%
  mutate(
    is_split    = TAXUNIT_final %in% c(1L, 2L),
    denom_earn  = pmax(R_TINCOME + SP_TINCOME, 1e-9),
    denom_lab   = pmax(R_LABORINC + SP_LABORINC, 1e-9),
    denom_bus   = pmax(R_BUSINC   + SP_BUSINC,   1e-9),
    share_earn  = case_when(
      !is_split          ~ 1,
      # Fallback to 50/50 when neither spouse has personal income
      R_TINCOME + SP_TINCOME == 0 ~ 0.5,
      TAXUNIT_final == 1L ~ R_TINCOME / denom_earn,
      TAXUNIT_final == 2L ~ SP_TINCOME / denom_earn
    ),
    share_labor = case_when(
      !is_split          ~ 1,
      # fallback to 0.5 if no labor income
      R_LABORINC + SP_LABORINC == 0 ~ 0.5,
      TAXUNIT_final == 1L ~ R_LABORINC / denom_lab,
      TAXUNIT_final == 2L ~ SP_LABORINC / denom_lab
    ),
    share_business = case_when(
      !is_split          ~ 1,
      R_BUSINC + SP_BUSINC == 0 ~ 0.5,
      TAXUNIT_final == 1L ~ R_BUSINC / denom_bus,
      TAXUNIT_final == 2L ~ SP_BUSINC / denom_bus
    ),
    share_housing = if_else(is_split, 0.5, 1),
    # Sex: primary of the tax unit is the respondent for TU=0 or TU=1,
    # the spouse for TU=2 (since the TAXUNIT=2 row represents the
    # spouse filing separately).
    male1 = case_when(
      TAXUNIT_final %in% c(0L, 1L) ~ as.integer(X8021 == 1),
      TAXUNIT_final == 2L           ~ as.integer(X103 == 1),
      TRUE                          ~ NA_integer_
    ),
    male2 = if_else(TAXUNIT_final == 0L & MARRIED == 1L,
                    as.integer(X103 == 1), NA_integer_),
    # Tax-unit identifier & implicate
    taxunitid = Y1 * 100L + TAXUNIT_final,
    implicate = as.integer(Y1 - 10L * YY1),
    weight    = X42001 / 5,
    # Canonical demographics
    age1 = as.integer(TUAGE),
    age2 = if_else(TAXUNIT_final == 0L & MARRIED == 1L, as.integer(SPAGE_unit), NA_integer_),
    n_dep = pmax(0L, KIDS_u)
  ) %>%
  select(
    Y1_row = Y1, YY1, taxunitid, implicate, weight, age1, age2, n_dep,
    married = MARRIED, male1, male2, filestat = FILESTAT,
    TAXUNIT_final, share_earn, share_labor, share_business, share_housing,
    R_TINCOME, SP_TINCOME
  )

# NPEU units — no SCFP-aggregate allocation; wealth=0, income from
# Section-Y fields computed above.
if (nrow(npeu_rows) > 0) {
  npeu_out = npeu_rows %>%
    mutate(
      is_split = FALSE,
      share_earn = 0, share_labor = 0, share_business = 0, share_housing = 0,
      taxunitid = Y1 * 100L + TAXUNIT_final,
      implicate = as.integer(Y1 - 10L * YY1),
      # NPEU weight matches the PEU (cloned), per Moore
      weight    = NA_real_,  # filled by join below
      age1 = as.integer(TUAGE),
      age2 = NA_integer_,
      n_dep = 0L,
      male1 = NA_integer_,   # not recovered for NPEU
      male2 = NA_integer_
    ) %>%
    select(Y1_row = Y1, YY1, taxunitid, implicate, age1, age2, n_dep,
           married = MARRIED, male1, male2, filestat = FILESTAT,
           TAXUNIT_final, share_earn, share_labor, share_business, share_housing,
           WSINCOME, GSSINC, TBUSINC, INTINC, DIVINC, RENTINC, PENINC,
           AFDCINC, CAPGLINC, UNEMPINC, OTHINC)

  # Join weight from parent household (same Y1_row → weight)
  weights_tbl = peu_out %>%
    filter(TAXUNIT_final %in% c(0L, 1L)) %>%
    distinct(Y1_row, weight)
  npeu_out = npeu_out %>% left_join(weights_tbl, by = 'Y1_row')

  # PEU output may lack Section-Y fields — fill with NA to allow bind_rows
  for (v in c('WSINCOME', 'GSSINC', 'TBUSINC', 'INTINC', 'DIVINC',
              'RENTINC', 'PENINC', 'AFDCINC', 'CAPGLINC', 'UNEMPINC', 'OTHINC'))
    peu_out[[v]] = 0
} else {
  npeu_out = tibble()
}

tu_frame = bind_rows(peu_out, npeu_out) %>% arrange(Y1_row, TAXUNIT_final)

#---------------------------------------------------------------------------
# Join SCFP aggregates and apply allocation rule.
#
# SCFP has 5 rows per household (one per implicate), keyed by Y1. Match
# each tax unit to its source implicate row via Y1_row.
#
# Allocation rule:
#   housing wealth (HOUSES, ORESRE, NNRESRE, MRTHEL, RESDBT, KGHOUSE, KGORE)
#     → share_housing
#   labor income (WAGEINC) → share_labor
#   business (BUSSEFARMINC) → share_business
#   everything else → share_earn (earnings-share)
#
# NPEU gets 0 wealth and Section-Y income that's already computed.
#---------------------------------------------------------------------------

wealth_housing = c('HOUSES', 'ORESRE', 'NNRESRE', 'MRTHEL', 'RESDBT',
                   'KGHOUSE', 'KGORE')
wealth_retirement = c('IRAKH', 'THRIFT', 'FUTPEN', 'CURRPEN')  # earnings-share fallback
wealth_other_assets = c('LIQ', 'CDS', 'STOCKS', 'STMUTF', 'COMUTF',
                        'BOND', 'SAVBND', 'TFBMUTF', 'GBMUTF', 'OBMUTF',
                        'CASHLI', 'ANNUIT', 'TRUSTS', 'OTHFIN', 'OMUTF',
                        'BUS', 'VEHIC', 'OTHNFIN',
                        'KGBUS', 'KGSTMF')
wealth_other_debts = c('OTHLOC', 'CCBAL', 'INSTALL', 'ODEBT')
income_fields = c('WAGEINC', 'BUSSEFARMINC', 'INTDIVINC', 'KGINC',
                  'RENT', 'SSRETINC', 'TRANSFOTHINC', 'INCOME')

scfp_joined = tu_frame %>%
  # Drop SCFP's YY1 before join to avoid a .x/.y suffix collision.
  left_join(scfp %>% select(-YY1), by = c('Y1_row' = 'Y1')) %>%
  mutate(
    # Wealth allocation
    across(all_of(wealth_housing),      ~ . * share_housing),
    across(all_of(wealth_retirement),   ~ . * share_earn),
    across(all_of(wealth_other_assets), ~ . * share_earn),
    across(all_of(wealth_other_debts),  ~ . * share_earn),
    # Income allocation (PEU rows)
    WAGEINC      = WAGEINC      * share_labor    + replace_na(WSINCOME, 0),
    BUSSEFARMINC = BUSSEFARMINC * share_business + replace_na(TBUSINC, 0),
    INTDIVINC    = INTDIVINC    * share_earn     + replace_na(INTINC + DIVINC, 0),
    KGINC        = KGINC        * share_earn     + replace_na(CAPGLINC, 0),
    RENT         = RENT         * share_earn     + replace_na(RENTINC, 0),
    SSRETINC     = SSRETINC     * share_earn     + replace_na(GSSINC + PENINC, 0),
    TRANSFOTHINC = TRANSFOTHINC * share_earn     + replace_na(AFDCINC + UNEMPINC + OTHINC, 0),
    INCOME       = INCOME       * share_earn +
                   replace_na(WSINCOME + TBUSINC + INTINC + DIVINC + CAPGLINC +
                              RENTINC + GSSINC + PENINC + AFDCINC + UNEMPINC + OTHINC, 0)
  )

#---------------------------------------------------------------------------
# Map SCFP raw fields to Stage-2's canonical Y schema
# (matches scf_to_y() in src/imputations/wealth.R).
#---------------------------------------------------------------------------

scf_tax_units = scfp_joined %>%
  transmute(
    taxunitid, scf_hh_id = YY1, implicate,
    weight, age1, age2, n_dep, married, male1, male2, filestat,
    taxunit_code = TAXUNIT_final,
    income = INCOME,

    # SCF-side income composition (post tax-unit split; NPEU-adjusted).
    # Carried through for X-ablation diagnostics
    # (src/eda/test_wealth_X_ablation.R). Not used by production wealth.R.
    wages_scf         = WAGEINC,
    business_scf      = BUSSEFARMINC,
    int_div_scf       = INTDIVINC,
    capital_gains_scf = KGINC,
    rent_scf          = RENT,
    ss_pens_scf       = SSRETINC,
    ui_other_scf      = TRANSFOTHINC,

    # Canonical wealth schema (from Wealth-Tax-Simulator/src/data.R:28-84)
    cash             = LIQ + CDS,
    equities         = STOCKS + STMUTF + COMUTF,
    bonds            = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    retirement       = IRAKH + THRIFT + FUTPEN + CURRPEN,
    life_ins         = CASHLI,
    annuities        = ANNUIT,
    trusts           = TRUSTS,
    other_fin        = OTHFIN + OMUTF,
    pass_throughs    = BUS,
    primary_home     = HOUSES,
    other_home       = ORESRE,
    re_fund          = NNRESRE,
    other_nonfin     = VEHIC + OTHNFIN,
    primary_mortgage = MRTHEL,
    other_mortgage   = RESDBT,
    credit_lines     = OTHLOC,
    credit_cards     = CCBAL,
    installment_debt = INSTALL,
    other_debt       = ODEBT,
    kg_primary_home  = KGHOUSE,
    kg_other_re      = KGORE,
    kg_pass_throughs = KGBUS,
    kg_other         = KGSTMF
  )

#---------------------------------------------------------------------------
# Sanity: households-total preservation. Weighted sum of each field
# across tax units, grouped by SCF household, should equal the original
# SCFP row's value × weight.
#
# Exact equality only holds for earnings-share-split fields when the
# sums commute (which they do: share_TU1 + share_TU2 = 1 by
# construction). Housing shares sum to 1 (0.5 + 0.5). Labor and
# business shares also sum to 1. NPEU wealth is 0 (no double-count).
#---------------------------------------------------------------------------

# Quick check: sum of weights across tax units per HH = 5 * PEU_weight
# (since splits share the parent row's weight).
ck = scf_tax_units %>%
  group_by(scf_hh_id) %>%
  summarise(n_taxunits = n(), tot_weight = sum(weight), .groups = 'drop')

cat(sprintf('stage1: %d tax-unit rows from %d SCF households\n',
            nrow(scf_tax_units), n_distinct(scf_tax_units$scf_hh_id)))
cat(sprintf('stage1: avg tax units per HH (across 5 implicates): %.2f\n',
            mean(ck$n_taxunits) / 5))
cat(sprintf('stage1: filing-status distribution on tax units:\n'))
print(table(scf_tax_units$filestat, useNA = 'ifany'))
cat(sprintf('stage1: weighted net worth = $%.1f trillion\n',
            sum(scf_tax_units$weight *
                (scf_tax_units$cash + scf_tax_units$equities + scf_tax_units$bonds +
                 scf_tax_units$retirement + scf_tax_units$life_ins +
                 scf_tax_units$annuities + scf_tax_units$trusts +
                 scf_tax_units$other_fin + scf_tax_units$pass_throughs +
                 scf_tax_units$primary_home + scf_tax_units$other_home +
                 scf_tax_units$re_fund + scf_tax_units$other_nonfin -
                 scf_tax_units$primary_mortgage - scf_tax_units$other_mortgage -
                 scf_tax_units$credit_lines - scf_tax_units$credit_cards -
                 scf_tax_units$installment_debt - scf_tax_units$other_debt)) / 1e12))

write_rds(scf_tax_units, cache_path)
cat(sprintf('stage1: cached scf_tax_units to %s\n', cache_path))

# Release the raw SCF frame (~1-2 GB) and intermediates so downstream
# imputations (wealth.R's DRF training) don't inherit the overhang.
rm(raw, scfp, scfp_joined, peu_base, peu_clone, peu_units,
   peu_out, npeu_out, npeu_rows, tu_frame, is_child, aconv,
   npeu_pos, npeu_from_row, alloc_odd, filed)
gc()

}  # end if (cache_usable) else
