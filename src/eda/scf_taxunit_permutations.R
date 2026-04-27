#---------------------------------------------
# scf_taxunit_permutations.R
#
# Reruns stage1 (build_scf_tax_units) under a
# fixed set of DoF variants and compares each
# variant's (age_bin × income-cell) joint
# distribution to the PUF's, restricted to the
# wealth-imputation universe (dep_status == 0).
#
# Variants tested:
#   baseline      — Moore's port verbatim
#   cohab         — relax X7370 cohabiting threshold (X7370 >= 0)
#   unclassified  — default unclassified records to TAXUNIT=1 (split)
#   kids1_split   — disable Moore's KIDS=1 inherit quirk
#   mfs_edge      — force split for X5746==2 + X7377!=2 + X7392!=2
#   all_on        — all four knobs on
#
# Output: plots/scf_taxunit_permutations/<ts>/
#   - report.md
#   - summary.csv
#   - joint_<variant>.csv
#   - gap_heatmap_<variant>.png
#
# Usage (must run via sbatch — never on login node):
#   sbatch slurm_taxunit_permutations.sh <output_dir>
# where <output_dir> is a baseline run dir containing
# tax_units_2022.csv (e.g.,
# /nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2025060311/baseline).
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(haven); library(data.table); library(purrr); library(Hmisc)
  library(ggplot2); library(stringr)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop('Usage: scf_taxunit_permutations.R <output_dir_with_tax_units_2022_csv>')
}
output_dir = args[1]
puf_path   = file.path(output_dir, 'tax_units_2022.csv')
if (!file.exists(puf_path)) stop('Cannot find ', puf_path)

# Hardcode SCF raw paths (matches the diagnostic-script convention; avoids
# sourcing configure.R which has side effects).
SCF_DIR   = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/2022/historical'
RAW_PATH  = file.path(SCF_DIR, 'p22i6.dta')
SCFP_PATH = file.path(SCF_DIR, 'SCFP2022.csv')

# Stage1's cache-check expects interface_paths$SCF when sourced. Stub it so
# we can pull in the function definitions without sourcing configure.R.
if (!exists('interface_paths')) {
  interface_paths = list(SCF = SCF_DIR)
}

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')
source('src/imputations/stage1_scf_tax_units.R')
# Stage1's source also (best-effort) loads scf_tax_units from cache. Save it
# for the regression check, then discard so we control the variable explicitly.
scf_tu_cached = if (exists('scf_tax_units')) scf_tax_units else NULL
if (exists('scf_tax_units')) rm(scf_tax_units)

#---------------------------------------------
# Bin definitions
#---------------------------------------------

AGE_BINS   = c(-1, 24, 34, 44, 54, 64, 74, 80)
AGE_LABELS = c('18-24','25-34','35-44','45-54','55-64','65-74','75-80')

#---------------------------------------------
# Variant set
#---------------------------------------------

variants = list(
  baseline = list(
    cohab_threshold = 1L, unclassified_taxunit = 0L,
    kids1_inherit = TRUE, mfs_edge_keep_joint = TRUE
  ),
  cohab = list(
    cohab_threshold = 0L, unclassified_taxunit = 0L,
    kids1_inherit = TRUE, mfs_edge_keep_joint = TRUE
  ),
  unclassified = list(
    cohab_threshold = 1L, unclassified_taxunit = 1L,
    kids1_inherit = TRUE, mfs_edge_keep_joint = TRUE
  ),
  kids1_split = list(
    cohab_threshold = 1L, unclassified_taxunit = 0L,
    kids1_inherit = FALSE, mfs_edge_keep_joint = TRUE
  ),
  mfs_edge = list(
    cohab_threshold = 1L, unclassified_taxunit = 0L,
    kids1_inherit = TRUE, mfs_edge_keep_joint = FALSE
  ),
  all_on = list(
    cohab_threshold = 0L, unclassified_taxunit = 1L,
    kids1_inherit = FALSE, mfs_edge_keep_joint = FALSE
  )
)

#---------------------------------------------
# Helpers
#---------------------------------------------

# Bin a tax-units frame to (age_bin × cell). Income definition follows
# age_dist_scf_vs_puf.R for SCF (post-stage1 components). The per-frame
# percentile is computed within-frame so each variant lives on its own
# percentile grid (same as how Stage 3 binning works).
bin_scf = function(tu) {
  d = tu %>% mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    income_calc = wages_scf + business_scf + int_div_scf + capital_gains_scf +
                  rent_scf + ss_pens_scf + ui_other_scf,
    age_bin = cut(age_older, breaks = AGE_BINS, labels = AGE_LABELS,
                   right = TRUE, include.lowest = TRUE)
  )
  ord = order(d$income_calc)
  cum_w = cumsum(d$weight[ord]) / sum(d$weight)
  rank_v = numeric(nrow(d)); rank_v[ord] = 100 * cum_w
  d$cell = CALIB_INCOME_BUCKETS[findInterval(rank_v, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]
  d
}

bin_puf = function(puf) {
  d = puf %>% mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    income = wages + sole_prop + farm +
             scorp_active  - scorp_active_loss  - scorp_179 +
             scorp_passive - scorp_passive_loss +
             part_active   - part_active_loss   - part_179 +
             part_passive  - part_passive_loss +
             txbl_int + exempt_int + div_ord + div_pref +
             kg_lt + kg_st + gross_ss + gross_pens_dist + ui +
             rent - rent_loss + estate - estate_loss,
    age_bin = cut(age_older, breaks = AGE_BINS, labels = AGE_LABELS,
                   right = TRUE, include.lowest = TRUE)
  )
  ord = order(d$income)
  cum_w = cumsum(d$weight[ord]) / sum(d$weight)
  rank_v = numeric(nrow(d)); rank_v[ord] = 100 * cum_w
  d$cell = CALIB_INCOME_BUCKETS[findInterval(rank_v, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]
  d
}

joint_pop = function(d) {
  d %>%
    group_by(age_bin, cell) %>%
    summarise(pop = sum(weight), .groups = 'drop') %>%
    mutate(
      age_bin = factor(age_bin, levels = AGE_LABELS),
      cell    = factor(cell,    levels = CALIB_INCOME_BUCKETS)
    )
}

compare_to_puf = function(scf_jt, puf_jt) {
  full_join(scf_jt %>% rename(scf_pop = pop),
            puf_jt %>% rename(puf_pop = pop),
            by = c('age_bin', 'cell')) %>%
    mutate(
      scf_pop = replace_na(scf_pop, 0),
      puf_pop = replace_na(puf_pop, 0),
      gap     = puf_pop - scf_pop
    )
}

summary_row = function(cmp, variant_name, runtime_s) {
  total_scf = sum(cmp$scf_pop)
  total_puf = sum(cmp$puf_pop)
  l1 = sum(abs(cmp$gap))
  top3 = cmp %>% arrange(desc(abs(gap))) %>% slice_head(n = 3)
  fmt_cell = function(i) {
    if (i > nrow(top3)) return(NA_character_)
    sprintf('%s × %s (%+.2fM)', top3$age_bin[i], top3$cell[i], top3$gap[i] / 1e6)
  }
  tibble(
    variant       = variant_name,
    runtime_s     = round(runtime_s, 1),
    scf_total_M   = round(total_scf / 1e6, 2),
    puf_total_M   = round(total_puf / 1e6, 2),
    gap_total_M   = round((total_puf - total_scf) / 1e6, 2),
    l1_M          = round(l1 / 1e6, 2),
    top1_cell     = fmt_cell(1),
    top2_cell     = fmt_cell(2),
    top3_cell     = fmt_cell(3)
  )
}

gap_heatmap = function(cmp, variant_name) {
  ggplot(cmp, aes(x = cell, y = age_bin, fill = gap / 1e6)) +
    geom_tile(color = 'grey80') +
    geom_text(aes(label = sprintf('%+.1f', gap / 1e6)), size = 3) +
    scale_fill_gradient2(name = 'PUF − SCF\n(M units)',
                         low = 'navy', mid = 'white', high = 'firebrick',
                         midpoint = 0) +
    labs(title  = sprintf('Variant: %s', variant_name),
         subtitle = 'PUF (dep_status==0) minus SCF tax-unit pop, by age × income cell',
         x = 'Income cell', y = 'Age bin') +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank())
}

#---------------------------------------------
# Load PUF
#---------------------------------------------

cat('Reading PUF: ', puf_path, '\n', sep = '')
puf_full = read_csv(puf_path, show_col_types = FALSE)
cat(sprintf('  %d rows, weighted pop %.1fM (all rows)\n',
            nrow(puf_full), sum(puf_full$weight) / 1e6))

puf = puf_full %>% filter(dep_status == 0)
cat(sprintf('  after dep_status==0 filter: %d rows, weighted pop %.1fM\n',
            nrow(puf), sum(puf$weight) / 1e6))

puf_binned = bin_puf(puf)
puf_joint  = joint_pop(puf_binned)

#---------------------------------------------
# Run variants
#---------------------------------------------

results = list()
runtimes = numeric(length(variants))
names(runtimes) = names(variants)

for (variant_name in names(variants)) {
  cat(sprintf('\n--- variant: %s ---\n', variant_name))
  t0 = Sys.time()
  cfg = modifyList(default_stage1_config(), variants[[variant_name]])
  results[[variant_name]] = build_scf_tax_units(
    config    = cfg,
    raw_path  = RAW_PATH,
    scfp_path = SCFP_PATH,
    syear     = 2022L
  )
  runtimes[[variant_name]] = as.numeric(Sys.time() - t0, units = 'secs')
  cat(sprintf('  runtime: %.1fs, rows: %d, pop: %.1fM\n',
              runtimes[[variant_name]],
              nrow(results[[variant_name]]),
              sum(results[[variant_name]]$weight) / 1e6))
}

#---------------------------------------------
# Refactor regression check
#---------------------------------------------

if (!is.null(scf_tu_cached)) {
  cat('\n--- regression check: baseline vs cached ---\n')
  base = results$baseline
  if (nrow(base) != nrow(scf_tu_cached)) {
    cat(sprintf('  WARN: row count differs (baseline %d, cache %d)\n',
                nrow(base), nrow(scf_tu_cached)))
  } else {
    common_cols = intersect(names(base), names(scf_tu_cached))
    diffs = sapply(common_cols, function(col) {
      a = base[[col]]; b = scf_tu_cached[[col]]
      if (is.numeric(a) && is.numeric(b)) {
        max(abs(a - b), na.rm = TRUE)
      } else {
        sum(a != b, na.rm = TRUE)
      }
    })
    worst = which.max(diffs)
    cat(sprintf('  max diff across %d common columns: %g (col: %s)\n',
                length(common_cols), diffs[worst], names(diffs)[worst]))
    if (max(diffs, na.rm = TRUE) < 1e-9) {
      cat('  PASS: refactor reproduces cached output\n')
    } else {
      cat('  WARN: refactor diverges from cached output — investigate\n')
    }
  }
}

#---------------------------------------------
# Comparison frames + summary
#---------------------------------------------

comparisons = list()
summary_rows = list()
for (v in names(results)) {
  scf_jt = joint_pop(bin_scf(results[[v]]))
  comparisons[[v]] = compare_to_puf(scf_jt, puf_joint)
  summary_rows[[v]] = summary_row(comparisons[[v]], v, runtimes[[v]])
}
summary_df = bind_rows(summary_rows)

cat('\n--- summary ---\n')
print(as.data.frame(summary_df), row.names = FALSE)

#---------------------------------------------
# Write artifacts
#---------------------------------------------

TS = format(Sys.time(), '%Y%m%d_%H%M%S')
out_dir = file.path('plots', 'scf_taxunit_permutations', TS)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cat('\nWriting artifacts to ', out_dir, '\n', sep = '')

write_csv(summary_df, file.path(out_dir, 'summary.csv'))

for (v in names(comparisons)) {
  cmp = comparisons[[v]]
  cmp_out = cmp %>%
    mutate(scf_pop_M = round(scf_pop / 1e6, 3),
           puf_pop_M = round(puf_pop / 1e6, 3),
           gap_M     = round(gap / 1e6, 3)) %>%
    select(age_bin, cell, scf_pop_M, puf_pop_M, gap_M) %>%
    arrange(age_bin, cell)
  write_csv(cmp_out, file.path(out_dir, sprintf('joint_%s.csv', v)))

  ggsave(file.path(out_dir, sprintf('gap_heatmap_%s.png', v)),
         gap_heatmap(cmp, v),
         width = 9, height = 5.5, dpi = 120)
}

# md report
report_lines = c(
  '# SCF tax-unit construction permutations vs PUF',
  '',
  sprintf('Run: `%s`', TS),
  sprintf('PUF source: `%s`', puf_path),
  '- Filter: `dep_status == 0` (the wealth-imputation universe; dep_status==1',
  '  rows have wealth zeroed in `wealth.R:710-716`)',
  sprintf('- Filtered PUF pop: %.1fM tax units (from %.1fM total)',
          sum(puf$weight) / 1e6, sum(puf_full$weight) / 1e6),
  '',
  '## Summary',
  '',
  '| variant | runtime_s | scf_total_M | puf_total_M | gap_total_M | l1_M | top1_cell | top2_cell | top3_cell |',
  '|---|---:|---:|---:|---:|---:|---|---|---|',
  paste0('| ', summary_df$variant,
         ' | ', summary_df$runtime_s,
         ' | ', summary_df$scf_total_M,
         ' | ', summary_df$puf_total_M,
         ' | ', summary_df$gap_total_M,
         ' | ', summary_df$l1_M,
         ' | ', summary_df$top1_cell,
         ' | ', summary_df$top2_cell,
         ' | ', summary_df$top3_cell, ' |'),
  '',
  '`gap_total_M = puf_total_M − scf_total_M`. `l1_M = sum |puf_cell_pop − scf_cell_pop|` (M tax units, summed across all 7×8 = 56 cells).',
  '',
  '## Variant definitions',
  '',
  '| variant | cohab_threshold | unclassified_taxunit | kids1_inherit | mfs_edge_keep_joint |',
  '|---|---|---|---|---|',
  paste0('| ', names(variants),
         ' | ', sapply(variants, function(v) v$cohab_threshold),
         ' | ', sapply(variants, function(v) v$unclassified_taxunit),
         ' | ', sapply(variants, function(v) v$kids1_inherit),
         ' | ', sapply(variants, function(v) v$mfs_edge_keep_joint), ' |'),
  '',
  '## Per-variant artifacts',
  '',
  unlist(lapply(names(comparisons), function(v) c(
    sprintf('### %s', v),
    sprintf('- joint table: [`joint_%s.csv`](joint_%s.csv)', v, v),
    sprintf('- gap heatmap: ![](gap_heatmap_%s.png)', v),
    ''
  )))
)
writeLines(report_lines, file.path(out_dir, 'report.md'))

cat(sprintf('\nDone. Output dir: %s\n', out_dir))
cat(sprintf('  report:  %s/report.md\n', out_dir))
cat(sprintf('  summary: %s/summary.csv\n', out_dir))
