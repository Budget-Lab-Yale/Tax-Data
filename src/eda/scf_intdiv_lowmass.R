#---------------------------------------------
# scf_intdiv_lowmass.R
#
# How many SCF tax units sit in the small-dollar
# int_div mass (0, 100] vs (0, 200]? Informs
# whether a symmetric threshold (SCF and PUF both
# zeroed below T) would also meaningfully cut
# SCF share_positive.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(Hmisc)
})

scf = read_rds('resources/cache/scf_tax_units.rds')

w_tot = sum(scf$weight)

cat(sprintf('SCF total pop:               %.2fM\n', w_tot / 1e6))
cat(sprintf('SCF holders (int_div > 0):   %.2fM (%.1f%%)\n',
            sum(scf$weight[scf$int_div_scf > 0]) / 1e6,
            100 * sum(scf$weight[scf$int_div_scf > 0]) / w_tot))

cat('\nSmall-dollar mass in SCF (weighted):\n')
for (T in c(10, 25, 50, 100, 150, 200, 300, 500, 1000)) {
  in_band = scf$int_div_scf > 0 & scf$int_div_scf <= T
  n_band  = sum(scf$weight[in_band])
  n_above = sum(scf$weight[scf$int_div_scf > T])
  cat(sprintf('  0 < int_div <= $%4d : %5.2fM (%4.2f%% of pop, %4.1f%% of holders) ; share_pos > T: %4.1f%%\n',
              T, n_band / 1e6, 100 * n_band / w_tot,
              100 * n_band / sum(scf$weight[scf$int_div_scf > 0]),
              100 * n_above / w_tot))
}

cat('\nRaw (unweighted) SCF sample sizes in bands:\n')
for (T in c(100, 200, 500)) {
  n = sum(scf$int_div_scf > 0 & scf$int_div_scf <= T)
  cat(sprintf('  0 < int_div <= $%4d : %d SCF rows\n', T, n))
}

cat('\nFor reference, PUF small-dollar mass (weighted):\n')
# Quick comparison using the same threshold
# Don't have puf here without CSV read — skip; this is already in the earlier probe.
cat('  (see slurm_nonfiler_intdiv.out for PUF threshold sweep)\n')

cat('\nDone.\n')
