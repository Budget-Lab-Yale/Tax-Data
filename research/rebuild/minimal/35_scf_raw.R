#--------------------------------------
# 35_scf_raw.R  (minimal example, step 35: business income, PUF against SCF;
# note 35_BUSINESS_INCOME.md)
#
# What it does: reads the raw 2016 SCF interview file (p16i6.dta, all five
# implicates) and writes
#   step35/scf2016_labels.tsv      the Stata variable label of every column
#                                  and its value labels where present
#   step35/scf2016_hh_items.csv.gz household-implicate rows with the income
#                                  items X5701-X5729, and every column whose
#                                  label mentions business, self-employment,
#                                  farm, partnership, S/C corporation, rent,
#                                  royalty, trust, estate, schedule or tax
#                                  return (the list is in the labels file,
#                                  column 'extracted')
# Nothing is recoded: the values are as in the file, codes included.
# Run: sbatch -J scfraw35 research/rebuild/minimal/35_cpu.sbatch scfraw
#--------------------------------------

suppressPackageStartupMessages({ library(haven); library(data.table) })

OUT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/step35'
raw_path = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/2016/historical/p16i6.dta'
stopifnot(file.exists(raw_path))
hh = read_dta(raw_path)
cat(sprintf('read %d rows, %d columns\n', nrow(hh), ncol(hh)))

lab = vapply(names(hh), function(v) paste(attr(hh[[v]], 'label'), collapse = ' '), '')
vlab = vapply(names(hh), function(v) {
  lb = attr(hh[[v]], 'labels')
  if (is.null(lb)) '' else paste(paste(lb, names(lb), sep = '='), collapse = '; ')
}, '')
pat = paste0('business|self-emp|self emp|farm|partner|corporation|s-corp|s corp|rent|royalt|',
             'trust|estate|schedule|tax return|1040|proprietor|practice|ranch|landlord|',
             'real estate|property|depreciat|net income|loss|profit|draw|salary')
income_block = sprintf('x%d', 5701:5729)
hit = names(hh)[grepl(pat, lab, ignore.case = TRUE)]
# Named items from the 2016 codebook (35_BUSINESS_INCOME.md, part 2): tax
# filing and schedules, business sections, real estate income, labor section.
named = sprintf('x%d', c(5744, 5746, 5747:5750, 7758:7765, 7451, 7650, 6765, 6766,
                         3103, 3104, 3105, 3119, 3219, 3128, 3228, 3129, 3229, 3131, 3231,
                         3132, 3232, 3335, 3337, 3401, 3407, 3411, 3451, 3415, 3419, 3427,
                         3410, 3414, 3454, 3418, 3422, 3430, 1700, 1703, 1704, 1729, 1730,
                         1803, 1804, 1829, 1830, 2009, 2010, 2019, 2020, 4106, 4706,
                         4112, 4712, 4125, 4725, 4127, 4727, 4131, 4731, 4132, 4732, 5725))
keep = unique(c('y1', 'yy1', 'x42001', income_block, named, hit))
keep = keep[keep %in% names(hh)]
fwrite(data.table(variable = names(hh), label = lab, value_labels = vlab,
                  extracted = names(hh) %in% keep),
       file.path(OUT, 'scf2016_labels.tsv'), sep = '\t')
x = as.data.table(lapply(hh[keep], as.numeric))
na = sapply(x, function(c) sum(is.na(c)))
cat('columns extracted:', length(keep), '; columns with any missing value:',
    sum(na > 0), '\n')
if (any(na > 0)) print(na[na > 0])
fwrite(x, file.path(OUT, 'scf2016_hh_items.csv.gz'))
cat('35_scf_raw: done\n')
