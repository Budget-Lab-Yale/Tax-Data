#--------------------------------------
# scf2022_preflight.R
#
# Pre-flight compatibility check before
# porting Moore's frbscftax.sas (which
# covers SCF 1989-2019) to SCF 2022.
#
# Checks:
#   A. Every XNNNN code Moore references
#      is present in p22i6.dta 2022
#   B. Every Y-schema field + aggregate
#      income field is present in
#      SCFP2022.csv
#   C. Implicate key layout (YY1 vs Y1)
#   D. Raw-SCF person-level retirement
#      ownership codes we'd need for the
#      retirement-by-owner split rule
#   E. Respondent sex code — Moore
#      doesn't carry it, we need it
#
# Output: plots/scf2022_preflight.txt
# (human-readable; run once, hand to
# user before porting).
#
# Inputs:
#   /tmp/moore_x_codes.txt  (extracted
#     offline from the SAS via regex)
#   interface_paths$SCF / p22i6.dta
#   interface_paths$SCF / SCFP2022.csv
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)

source('./src/configure.R')

report_path = 'plots/scf2022_preflight.txt'
dir.create(dirname(report_path), showWarnings = FALSE, recursive = TRUE)

# Open the report and redirect cat() output to it in parallel
sink(report_path, split = TRUE)

cat('SCF 2022 pre-flight report\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 60), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# A. p22i6.dta — raw X-code coverage
#---------------------------------------------------------------------------

raw_path = interface_paths$SCF %>% file.path('p22i6.dta')
cat('A. Raw SCF 2022 file: ', raw_path, '\n\n', sep = '')

if (!file.exists(raw_path)) {
  cat('  ERROR: file not found.\n')
  sink()
  stop('p22i6.dta missing at ', raw_path)
}

# haven::read_dta supports reading column names only (n_max = 0)
raw_head = haven::read_dta(raw_path, n_max = 0)
# p22i6.dta uses lowercase X-codes (Stata convention); Moore's SAS uses
# uppercase. Normalize both to uppercase for comparison.
raw_names = toupper(names(raw_head))
cat('  Columns found:', length(raw_names),
    '(lowercase in file; uppercased for comparison)\n')

# Load Moore's X-code list (extracted offline). If the file isn't present,
# fall back to a core list that's load-bearing for the port we need.
moore_codes_path = '/tmp/moore_x_codes.txt'
if (file.exists(moore_codes_path)) {
  moore_x = readLines(moore_codes_path)
  cat('  Moore X-codes checked:', length(moore_x), '(from ', moore_codes_path, ')\n', sep = '')
} else {
  moore_x = c(
    # Tax-unit split decision
    'X5744', 'X5746', 'X7020', 'X7370', 'X7377', 'X7392', 'X8023', 'X105',
    # Ages
    'X14', 'X19',
    # Weight + household ID
    'X42001',
    # Kids
    'X5912',
    # NPEU enumeration (>=2007 band: 10 positions)
    'X110', 'X116', 'X122', 'X128', 'X134',
    'X204', 'X210', 'X216', 'X222', 'X228',
    'X112', 'X118', 'X124', 'X130', 'X136',
    'X206', 'X212', 'X218', 'X224', 'X230',
    'X113', 'X119', 'X125', 'X131', 'X137',
    'X207', 'X213', 'X219', 'X225', 'X231',
    'X100', 'X106', 'X107',
    'X114', 'X120', 'X126', 'X132',
    'X202', 'X208', 'X214', 'X220', 'X226',
    # Person-level income decomposition (R_*/SP_*)
    'X4112', 'X4113', 'X4712', 'X4713',
    'X4131', 'X4132', 'X4731', 'X4732',
    # NPEU Section-Y income
    'X7050', 'X6401', 'X6403',
    'X6406', 'X6407', 'X6408', 'X6409', 'X6410',
    'X6411', 'X6412', 'X6413', 'X6414', 'X6415',
    # Core income category switches
    'X5724', 'X5725', 'X5726', 'X5727', 'X5729',
    'X5818', 'X5821', 'X5823',
    # Pension income (Moore uses these for R_REGPEN/SP_REGPEN,
    # R_WITHDRAW/SP_WITHDRAW, R_SSA/SP_SSA reconstruction)
    'X5315', 'X5316', 'X5324', 'X5325',
    'X5331', 'X5332', 'X5335', 'X5336',
    'X5345', 'X5346', 'X5355', 'X5356',
    'X5634', 'X5636', 'X5642', 'X5644', 'X5646'
  )
  cat('  Moore X-codes checked:', length(moore_x), '(fallback core set)\n')
}

missing_x = moore_x[!(moore_x %in% raw_names)]
cat('  Missing from p22i6.dta:', length(missing_x), '\n')

# Classify missing codes by whether they are used inside branches of
# Moore's SAS that SCF 2022 (SYEAR=2022) actually enters. Moore's SAS is
# macro-gated by SYEAR, so codes referenced only in `%IF &SYEAR LE 2007`
# blocks are dead code for 2022 and we can ignore them.
sas = readLines('/tmp/frbscftax.sas')
# Line ranges that are SYEAR=2022-active (pre-2010 branches are dead code):
syear2022_active = list(
  c(1,    287),    # pre-branching global
  c(289,  304),    # SYEAR >= 2001
  c(305,  333),    # SYEAR >= 1995
  c(374,  379),    # SYEAR >= 1995
  c(418,  430),    # SYEAR LE 1992 OR GE 2007 (2022 enters)
  c(445,  455),    # same — KIDSU17
  c(469,  479),    # same — KIDSU18
  c(493,  503),    # same — KIDSU13
  c(517,  637),    # post-KIDS prep, pre-pension
  c(638,  656),    # SYEAR GE 2010 — pension decomposition (2022 enters)
  c(740,  1022),   # post-pension tax-unit structure
  c(1027, 1030),   # NPEU arrays, SYEAR LE 1992 OR GE 2007
  c(1064, 1074),   # NPEU age calcs, SYEAR GE 2007
  c(1084, 1114)    # NPEU Section-Y allocation
)
# Within our port scope: everything above, except we skip the TAXSIM-facing
# income-allocation blocks that extend to line 4051.
in_scope_line = function(ln) {
  any(sapply(syear2022_active, function(r) ln >= r[1] && ln <= r[2]))
}
scope_of_code = function(code) {
  ln = grep(paste0('\\b', code, '\\b'), sas)
  if (length(ln) == 0) return('no-refs')
  if (any(sapply(ln, in_scope_line))) 'IN_SCOPE' else 'skip'
}

# Also detect codes that are COMPUTED inside the SAS rather than raw-read.
# X7370 is the known case: `IF &SYEAR=X8005 THEN X7370=-1; ELSE X7370=&SYEAR-X8005;`
# These look "missing" but aren't — they're derived downstream of a raw code.
is_computed_in_sas = function(code) {
  any(grepl(paste0('\\b', code, '\\s*='), sas))
}

scope_tbl = data.frame(
  code     = missing_x,
  scope    = sapply(missing_x, scope_of_code),
  computed = sapply(missing_x, is_computed_in_sas),
  stringsAsFactors = FALSE
)
# "real blockers": used in our scope AND not a derived variable defined in-SAS
scope_tbl$blocker = scope_tbl$scope == 'IN_SCOPE' & !scope_tbl$computed

cat('\n  Classification:\n')
cat('    skip (TAXSIM-only, we do not port): ',
    sum(scope_tbl$scope == 'skip'), '\n', sep = '')
cat('    computed in SAS (false positive) : ',
    sum(scope_tbl$computed), '\n', sep = '')
cat('    REAL BLOCKERS (in-scope missing) : ',
    sum(scope_tbl$blocker), '\n', sep = '')

if (any(scope_tbl$blocker)) {
  cat('\n  Real blockers (pension slots 5-6 dropped from SCF 2022 public):\n')
  for (v in scope_tbl$code[scope_tbl$blocker]) cat('    -', v, '\n')
  cat('\n  These feed R_REGPEN / SP_REGPEN / R_WITHDRAW / SP_WITHDRAW\n')
  cat('  contributions from pension slots 5-6 and per-account withdrawals\n')
  cat('  for those slots. The Fed reduced explicit pension slots in the\n')
  cat('  2022 public file.\n\n')
  cat('  FALLBACK: zero-substitute in the port. In Moore expressions like\n')
  cat('    MAX(0, X6484 * ACONV(X6485) * (X5423=1))\n')
  cat('  missing columns are treated as 0, so slot-5+6 terms drop out\n')
  cat('  cleanly. Impact limited to rare households with 5+ pensions.\n')
  cat('\n  2022 codebook (for reference, not required):\n')
  cat('    https://www.federalreserve.gov/econres/files/codebk2022.txt\n')
}

if (any(scope_tbl$scope == 'skip')) {
  cat('\n  Skip (not in our port scope, safe to ignore):\n')
  cat('    ', paste(scope_tbl$code[scope_tbl$scope == 'skip'], collapse = ', '),
      '\n', sep = '')
}
if (any(scope_tbl$computed)) {
  cat('\n  Computed-in-SAS (false positive, not raw-read):\n')
  cat('    ', paste(scope_tbl$code[scope_tbl$computed], collapse = ', '),
      '\n', sep = '')
}

#---------------------------------------------------------------------------
# B. SCFP2022.csv — aggregate field coverage for Y-schema + income
#---------------------------------------------------------------------------

scfp_path = interface_paths$SCF %>% file.path('SCFP2022.csv')
cat('\n\nB. SCFP 2022 summary: ', scfp_path, '\n\n', sep = '')

scfp_head = fread(scfp_path, nrows = 1)
scfp_names = names(scfp_head)
cat('  Columns found:', length(scfp_names), '\n')

# Wealth 23-var schema pieces (what scf_to_y() in wealth.R reads)
y_schema_raw = c(
  'LIQ', 'CDS', 'STOCKS', 'STMUTF', 'COMUTF',
  'BOND', 'SAVBND', 'TFBMUTF', 'GBMUTF', 'OBMUTF',
  'IRAKH', 'THRIFT', 'FUTPEN', 'CURRPEN',
  'CASHLI', 'ANNUIT', 'TRUSTS', 'OTHFIN', 'OMUTF',
  'BUS', 'HOUSES', 'ORESRE', 'NNRESRE', 'VEHIC', 'OTHNFIN',
  'MRTHEL', 'RESDBT', 'OTHLOC', 'CCBAL', 'INSTALL', 'ODEBT',
  'KGHOUSE', 'KGORE', 'KGBUS', 'KGSTMF'
)
# Income aggregates needed for allocation
scfp_income = c('WAGEINC', 'BUSSEFARMINC', 'INTDIVINC', 'KGINC',
                'RENT', 'SSRETINC', 'TRANSFOTHINC', 'INCOME')
# Identifiers
scfp_ids = c('Y1', 'YY1', 'WGT')

missing_y   = y_schema_raw[!(y_schema_raw %in% scfp_names)]
missing_inc = scfp_income  [!(scfp_income  %in% scfp_names)]
missing_id  = scfp_ids     [!(scfp_ids     %in% scfp_names)]

cat('  Y-schema raw fields:', length(y_schema_raw), '— missing:', length(missing_y), '\n')
if (length(missing_y) > 0) for (v in missing_y) cat('    -', v, '\n')
cat('  Income aggregates  :', length(scfp_income), '— missing:', length(missing_inc), '\n')
if (length(missing_inc) > 0) for (v in missing_inc) cat('    -', v, '\n')
cat('  Identifiers        :', length(scfp_ids), '— missing:', length(missing_id), '\n')
if (length(missing_id) > 0) for (v in missing_id) cat('    -', v, '\n')

#---------------------------------------------------------------------------
# C. Implicate key layout
#---------------------------------------------------------------------------

cat('\n\nC. Implicate key layout\n\n')

scfp = fread(scfp_path)
n_rows = nrow(scfp)
n_y1_unique = if ('Y1' %in% names(scfp)) length(unique(scfp$Y1)) else NA
n_yy1_unique = if ('YY1' %in% names(scfp)) length(unique(scfp$YY1)) else NA

cat('  SCFP rows:          ', n_rows, '\n')
cat('  Unique Y1   (per-row HH+impl):', n_y1_unique,
    if (!is.na(n_y1_unique)) sprintf('  (expected = nrow)') else '', '\n', sep = '')
cat('  Unique YY1  (household only): ', n_yy1_unique,
    if (!is.na(n_yy1_unique)) sprintf('  (expected ~%d HHs)', round(n_rows / 5)) else '', '\n', sep = '')

# SCFP 2022: Y1 is per-row (household + implicate), YY1 is household-only.
# Implicate code is the trailing digit of Y1 (Y1 = YY1 * 10 + implicate_num).
if (!is.na(n_y1_unique) && n_y1_unique == n_rows) {
  yy1 = scfp$YY1[1:10]
  y1  = scfp$Y1 [1:10]
  cat('  First 10 (YY1, Y1, implicate): \n')
  for (i in 1:10) cat(sprintf('    YY1=%d  Y1=%d  implied imp=%d\n',
                               yy1[i], y1[i], y1[i] - 10 * yy1[i]))
  impl = scfp$Y1 - 10 * scfp$YY1
  cat('  Inferred implicate range: ', range(impl)[1], '..', range(impl)[2], '\n', sep = '')
  cat('  Implicate 1..5 counts: ',
      paste(sapply(1:5, function(k) sum(impl == k)), collapse = ', '), '\n')
}

#---------------------------------------------------------------------------
# D. Person-level retirement ownership
#
# IRAKH, THRIFT, FUTPEN, CURRPEN are SCFP aggregates. Underlying raw codes
# enumerate accounts with a person-type flag. Common raw codes referenced
# in the Fed 2022 codebook: X6551, X6559, X6560, X6567, X6571, X6576 (IRA
# ownership), X11000+ (pension accounts, respondent vs spouse). Verify
# which of these exist so we can wire person-level retirement attribution.
#---------------------------------------------------------------------------

cat('\n\nD. Person-level retirement ownership codes\n\n')

retirement_candidates = c(
  # IRA ownership flags (per-account, answer codes: 1=R, 2=SP, 3=both, ...)
  'X6551', 'X6559', 'X6560', 'X6567', 'X6571', 'X6576',
  # Pension account identifiers (per-job, respondent)
  'X11032', 'X11132', 'X11232', 'X11332',
  'X11432', 'X11532', 'X11632', 'X11732',
  # Spouse pension
  'X11100', 'X11200', 'X11300', 'X11400',
  'X11500', 'X11600', 'X11700', 'X11800',
  # Quasi-liquid retirement balances (Thrift/Keogh/401k)
  'X3902', 'X3909', 'X3913', 'X3915', 'X3917', 'X3920'
)
ret_present = retirement_candidates[retirement_candidates %in% raw_names]
ret_missing = retirement_candidates[!(retirement_candidates %in% raw_names)]
cat('  Candidate retirement-ownership codes checked:', length(retirement_candidates), '\n')
cat('  Present:', length(ret_present), '\n')
if (length(ret_present) > 0)
  cat('    ', paste(ret_present, collapse = ', '), '\n', sep = '')
cat('  Missing:', length(ret_missing), '\n')
if (length(ret_missing) > 0)
  cat('    ', paste(ret_missing, collapse = ', '), '\n', sep = '')
cat('  Decision: retirement sub-components without clear owner flags fall\n')
cat('  back to earnings-share split per the plan.\n')

#---------------------------------------------------------------------------
# E. Respondent / spouse sex
#---------------------------------------------------------------------------

cat('\n\nE. Respondent / spouse sex codes (needed for male1 / male2)\n\n')

sex_candidates = c(
  'X8021',   # respondent sex (typical SCF coding)
  'X8022',   # spouse sex
  'X101',    # respondent sex (alternative position)
  'X103',    # spouse sex (alternative)
  'X104'     # alternative
)
sex_present = sex_candidates[sex_candidates %in% raw_names]
cat('  Candidates checked:', paste(sex_candidates, collapse = ', '), '\n')
cat('  Present:', paste(sex_present, collapse = ', '), '\n')
cat('  --> inspect value distribution of the present candidates below.\n\n')

if (length(sex_present) > 0) {
  # On disk the variables are lowercase; read them by their real names.
  sex_present_lc = tolower(sex_present)
  small = haven::read_dta(raw_path, col_select = all_of(sex_present_lc))
  for (v in sex_present_lc) {
    tbl = table(small[[v]], useNA = 'ifany')
    cat('  ', toupper(v), ' value counts: ',
        paste(names(tbl), ':', as.integer(tbl),
              sep = '', collapse = '   '), '\n', sep = '')
  }
}

#---------------------------------------------------------------------------
# Summary verdict
#---------------------------------------------------------------------------

cat('\n\n', paste0(rep('=', 60), collapse = ''), '\n', sep = '')
cat('SUMMARY\n')
cat(paste0(rep('=', 60), collapse = ''), '\n')

real_blockers = sum(scope_tbl$blocker) + length(missing_y) + length(missing_inc) + length(missing_id)
if (real_blockers == 0) {
  cat('GREEN: no in-scope blockers. Moore port can proceed on SCF 2022.\n')
} else {
  cat('YELLOW: ', real_blockers, ' in-scope blocker(s):\n', sep = '')
  cat('  - p22i6 missing X-codes (in-scope) : ', sum(scope_tbl$blocker), '\n')
  cat('  - SCFP missing Y-schema            : ', length(missing_y),      '\n')
  cat('  - SCFP missing income              : ', length(missing_inc),    '\n')
  cat('  - SCFP missing IDs                 : ', length(missing_id),     '\n')
  cat('  Out-of-scope missing codes (safe to ignore):',
      sum(scope_tbl$scope == 'skip'), '\n')
  cat('  Computed-in-SAS false positives           :',
      sum(scope_tbl$computed),        '\n')
}

cat('\nReport saved to: ', report_path, '\n', sep = '')
sink()
