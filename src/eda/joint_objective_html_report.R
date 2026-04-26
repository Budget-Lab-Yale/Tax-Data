#---------------------------------------------
# joint_objective_html_report.R
#
# Builds a self-contained HTML report explaining
# the Stage 3 Step A swap experiment in plain
# English, with grouped bar charts per
# (category × metric) comparing four sources of
# numbers per cell: SCF target, pre-swap (uniform
# random leaf draw), post-swap count-only, and
# post-swap joint (count + amount).
#
# Output: docs/swap_results.html (single file,
# all charts embedded inline as SVG).
#
# Usage:
#   Rscript src/eda/joint_objective_html_report.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(readr)
  library(tibble); library(Hmisc); library(htmltools); library(svglite)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L)
  stop('Usage: Rscript src/eda/joint_objective_html_report.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

estimate_models = 0L
do_lp           = 0L

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')


#--- Inputs -----------------------------------------------------------------

cat('Loading inputs...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')
snap_path = file.path(output_dir, 'puf_2022_snapshot.rds')
if (file.exists(snap_path)) {
  puf_2022 = read_rds(snap_path)
} else {
  puf_2022 = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                      show_col_types = FALSE)
  for (v in wealth_y_vars) puf_2022[[v]] = NA_real_
}
source('src/imputations/wealth.R')


#--- Run arms ---------------------------------------------------------------

run_arm = function(label, ...) {
  cat(sprintf('\n========= %s =========\n', label))
  set.seed(76)
  result = run_wealth_imputation(puf_2022, scf_tax_units, ...)
  result$arm = label
  result
}

res_count_only = run_arm('count_only',
                          stage3_method = 'swap', min_node_size = 50L,
                          swap_options  = list(amount_weight = 0))
res_joint      = run_arm('joint',
                          stage3_method = 'swap', min_node_size = 50L,
                          swap_options  = list(amount_weight = 0.5))


#--- Cell-keyed dataframes --------------------------------------------------

puf_w = puf_2022$weight
puf_age2 = ifelse(is.na(puf_2022$age2), 0L, puf_2022$age2)
puf_age_older = pmax(pmin(80L, puf_2022$age1), pmin(80L, puf_age2))
puf_income_vec = with(puf_2022,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st + gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)
puf_meta = data.frame(id = puf_2022$id, weight = puf_w,
                      age_older = puf_age_older, income = puf_income_vec)
puf_meta = assign_calibration_cells(puf_meta, puf_meta$income,
                                    puf_meta$age_older, puf_meta$weight)

attach_meta = function(df_y) {
  df_y %>% inner_join(puf_meta %>% select(id, weight, cell_income, cell_age),
                       by = 'id') %>%
    bind_cols(compute_category_values(.))
}

scf_y    = scf_to_y(scf_tax_units)
scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_ao   = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_meta = data.frame(weight = scf_y$weight, age_older = scf_ao, income = scf_inc)
scf_meta = cbind(scf_meta, compute_category_values(scf_y))
scf_meta = assign_calibration_cells(scf_meta, scf_meta$income,
                                    scf_meta$age_older, scf_meta$weight)


#--- Per-source per-cell stats ----------------------------------------------

CATS = c('nw','equities','bonds','homes','other','debt')

cell_cat_stats = function(df, label) {
  out = list()
  for (cc in CATS) {
    col = paste0('cat_', cc)
    s = df %>%
      group_by(cell_income, cell_age) %>%
      summarise(count_wt = sum(weight * (.data[[col]] > 0)),
                total    = sum(weight * .data[[col]]),
                .groups  = 'drop') %>%
      mutate(category = cc, source = label)
    out[[cc]] = s
  }
  bind_rows(out)
}

# pre-swap: uniform-init forest output (same in both arms — same seed).
pre_df = attach_meta(res_count_only$y_pre_tilt)
co_df  = attach_meta(res_count_only$y_post_step_a_pre_rescale)
jt_df  = attach_meta(res_joint$y_post_step_a_pre_rescale)

stats_long = bind_rows(
  cell_cat_stats(scf_meta, 'SCF target'),
  cell_cat_stats(pre_df,   'pre-swap (forest)'),
  cell_cat_stats(co_df,    'count-only swap'),
  cell_cat_stats(jt_df,    'joint swap')
)

# Order cells by income then age, with shorter labels for chart readability.
cell_order = expand.grid(
  cell_income = CALIB_INCOME_BUCKETS,
  cell_age    = CALIB_AGE_BUCKETS,
  stringsAsFactors = FALSE
)
cell_order = cell_order[order(match(cell_order$cell_income, CALIB_INCOME_BUCKETS),
                               cell_order$cell_age), ]
short_label = function(ci, ca) {
  ci_short = sub('pct', '', ci)
  ci_short = gsub('to', '–', ci_short)
  age_tag  = if (ca == 'senior') ' s' else ' n'
  paste0(ci_short, age_tag)
}
cell_levels = mapply(short_label, cell_order$cell_income, cell_order$cell_age,
                      USE.NAMES = FALSE)
stats_long$cell = factor(
  mapply(short_label, stats_long$cell_income, stats_long$cell_age,
         USE.NAMES = FALSE),
  levels = cell_levels)

stats_long$source = factor(stats_long$source,
  levels = c('SCF target', 'pre-swap (forest)', 'count-only swap', 'joint swap'))

source_colors = c(
  'SCF target'        = '#444444',
  'pre-swap (forest)' = '#a6cee3',
  'count-only swap'   = '#fdbf6f',
  'joint swap'        = '#33a02c'
)


#--- Helpers: ggplot → inline SVG -------------------------------------------

plot_to_svg = function(plt, width_in = 13, height_in = 4.2) {
  s = svglite::svgstring(width = width_in, height = height_in,
                          standalone = FALSE)
  print(plt)
  invisible(dev.off())
  paste(s(), collapse = '')
}

bar_chart = function(cat_name, metric, ylab) {
  d = stats_long %>% filter(category == cat_name) %>%
    mutate(value = if (metric == 'count') count_wt / 1e6
                   else                    total    / 1e12)

  ggplot(d, aes(x = cell, y = value, fill = source)) +
    geom_col(position = position_dodge(width = 0.85), width = 0.78) +
    scale_fill_manual(values = source_colors, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = ylab) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x       = element_text(angle = 0, size = 9),
      axis.title.y      = element_text(size = 10),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position   = 'top',
      legend.key.size   = unit(0.4, 'cm'),
      plot.margin       = margin(2, 8, 2, 4)
    )
}


#--- Match-quality summary --------------------------------------------------

err_for = function(df, label) {
  cell_cat_stats(df, label) %>%
    inner_join(cell_cat_stats(scf_meta, 'SCF') %>%
                 select(cell_income, cell_age, category,
                        count_scf = count_wt, total_scf = total),
                by = c('cell_income','cell_age','category')) %>%
    mutate(count_err = abs(count_wt - count_scf) / pmax(count_scf, 1),
           total_err = abs(total - total_scf) / pmax(abs(total_scf), 1))
}
e_co = err_for(co_df, 'count_only')
e_jt = err_for(jt_df, 'joint')

q_within = function(err, key, t) 100 * mean(err[[key]] <= t, na.rm = TRUE)
match_quality_tbl = tibble(
  metric = c('count match within ±5%', 'count match within ±10%',
             'amount match within ±5%', 'amount match within ±10%',
             'amount match within ±25%'),
  count_only = c(q_within(e_co, 'count_err', 0.05),
                  q_within(e_co, 'count_err', 0.10),
                  q_within(e_co, 'total_err', 0.05),
                  q_within(e_co, 'total_err', 0.10),
                  q_within(e_co, 'total_err', 0.25)),
  joint = c(q_within(e_jt, 'count_err', 0.05),
             q_within(e_jt, 'count_err', 0.10),
             q_within(e_jt, 'total_err', 0.05),
             q_within(e_jt, 'total_err', 0.10),
             q_within(e_jt, 'total_err', 0.25))
) %>% mutate(across(c(count_only, joint), ~ sprintf('%.1f%%', .x)))


#--- Step B factor summary --------------------------------------------------

rf_summary_one = function(rf) {
  r = rf %>% filter(applied)
  list(median = median(r$factor),
       median_log2 = median(abs(log2(pmax(r$factor, 1e-6)))),
       n_outside = sum(r$factor < 0.5 | r$factor > 2.0),
       max = max(r$factor), min = min(r$factor))
}
rf_co = rf_summary_one(res_count_only$rescale_factors)
rf_jt = rf_summary_one(res_joint$rescale_factors)


#--- Top NW shares (post-Step-B, what tax-simulator sees) -------------------

weighted_quantile = function(x, w, probs) {
  o = order(x); x = x[o]; w = w[o]
  p = cumsum(w) / sum(w); approx(p, x, xout = probs, rule = 2)$y
}
top_share = function(x, w, top_frac) {
  cutoff = weighted_quantile(x, w, 1 - top_frac)
  above  = x >= cutoff
  sum(w[above] * x[above]) / sum(w * x)
}

ts = function(df) c(
  top_001 = top_share(df$cat_nw, df$weight, 0.001),
  top_01  = top_share(df$cat_nw, df$weight, 0.01),
  top_1   = top_share(df$cat_nw, df$weight, 0.05),
  top_10  = top_share(df$cat_nw, df$weight, 0.10)
)
scf_ts = ts(scf_meta)
co_post_ts = ts(attach_meta(res_count_only$y))
jt_post_ts = ts(attach_meta(res_joint$y))

top_share_tbl = tibble(
  share = c('top 0.1%', 'top 1%', 'top 5%', 'top 10%'),
  scf       = sprintf('%.3f', scf_ts),
  count_only_pp = sprintf('%+.2f pp',
                           100 * (co_post_ts - scf_ts)),
  joint_pp      = sprintf('%+.2f pp',
                           100 * (jt_post_ts - scf_ts))
)


#--- Generate all 12 charts -------------------------------------------------

cat('\nGenerating charts...\n')

cat_explain = list(
  nw       = list(
    title = 'Net worth (NW = total assets − total debts)',
    note  = 'NW is the headline wealth measure. Note: the joint solver does not target nw amount directly (it is implicit, derived from the 5 component amount targets).'
  ),
  equities = list(
    title = 'Equities (publicly traded stocks and stock funds)',
    note  = 'A direct amount target. Joint swap fits SCF amount essentially exactly in the middle income cells.'
  ),
  bonds    = list(
    title = 'Bonds (taxable, tax-free, savings, mutual fund bonds)',
    note  = 'A direct amount target. Joint hits SCF amount essentially exactly in 14 of 16 cells.'
  ),
  homes    = list(
    title = 'Primary residence',
    note  = 'A direct amount target. Counts are donor-pool-limited in two senior cells (60–80 senior and 80–90 senior) — too few non-homeowner donors.'
  ),
  other    = list(
    title = 'Other assets (cash, retirement, business, other real estate, vehicles, …)',
    note  = '<strong>The hardest category.</strong> "Other" lumps 9 distinct wealth components into one aggregate. The solver has only one knob (the aggregate amount) for an aggregate determined by 9 underlying values. This is a structural limitation, not a solver issue.'
  ),
  debt     = list(
    title = 'Debt (mortgages, credit lines, credit cards, installment, other)',
    note  = 'A direct amount target. Joint hits SCF essentially exactly in 12 of 16 cells.'
  )
)

charts_html = lapply(CATS, function(cc) {
  ex = cat_explain[[cc]]
  count_chart  = bar_chart(cc, 'count',  'count of households (M)')
  amount_chart = bar_chart(cc, 'amount', 'amount ($T)')
  tagList(
    tags$h3(ex$title),
    tags$p(HTML(ex$note)),
    tags$h4('Households with positive holdings (count)'),
    HTML(plot_to_svg(count_chart)),
    tags$h4('Aggregate dollar amount'),
    HTML(plot_to_svg(amount_chart))
  )
})


#--- Build HTML body --------------------------------------------------------

cat('Building HTML...\n')

css = "
body { font-family: -apple-system, system-ui, sans-serif;
       max-width: 1100px; margin: 2em auto; padding: 0 1.2em;
       line-height: 1.55; color: #222; }
h1 { font-size: 1.8em; line-height: 1.2; margin-top: 0; }
h2 { font-size: 1.35em; line-height: 1.2; margin-top: 1.6em;
     padding-bottom: 0.2em; border-bottom: 1px solid #ddd; }
h3 { font-size: 1.1em; margin-top: 1.4em; }
h4 { font-size: 0.95em; margin-top: 1.0em; color: #555; }
p, li { font-size: 0.96em; }
table { border-collapse: collapse; margin: 1em 0;
        font-size: 0.92em; }
th, td { border: 1px solid #ccc; padding: 0.35em 0.7em; text-align: right; }
th { background: #f5f5f5; text-align: left; }
td:first-child, th:first-child { text-align: left; }
.small { font-size: 0.9em; color: #555; }
.note { background: #fffbe6; border-left: 4px solid #f0c040;
        padding: 0.6em 1em; margin: 1em 0; font-size: 0.93em; }
.legend-key { display: inline-block; width: 1em; height: 1em;
              vertical-align: middle; margin-right: 0.3em; border: 1px solid #888; }
"

quality_tbl_html = function(tbl) {
  rows = lapply(seq_len(nrow(tbl)), function(i) {
    tags$tr(lapply(names(tbl), function(col) tags$td(tbl[[col]][i])))
  })
  tags$table(
    tags$thead(tags$tr(lapply(names(tbl), tags$th))),
    tags$tbody(rows)
  )
}

intro_legend = HTML('
<p>The bar charts use four colors per cell:</p>
<ul>
  <li><span class="legend-key" style="background: #444444;"></span><strong>SCF target</strong> — what the SCF says the right answer is.</li>
  <li><span class="legend-key" style="background: #a6cee3;"></span><strong>pre-swap (forest)</strong> — the raw output from the random forest (uniform random pick of one donor per PUF record from its leaf). This is the starting point.</li>
  <li><span class="legend-key" style="background: #fdbf6f;"></span><strong>count-only swap</strong> — after running the swap solver with the original objective (matches counts only).</li>
  <li><span class="legend-key" style="background: #33a02c;"></span><strong>joint swap</strong> — after running the swap solver with the new objective (matches both counts AND amounts).</li>
</ul>
')

cell_label_help = HTML('
<p class="small">Cell labels: e.g. <code>20–40 n</code> = income 20th–40th percentile, non-senior;
<code>99.9–100 s</code> = top 0.1% income, senior. <code>n</code> = non-senior, <code>s</code> = senior.</p>
')

html = tags$html(
  tags$head(
    tags$meta(charset = 'utf-8'),
    tags$title('Stage 3 Step A: swap experiment results (plain English)'),
    tags$style(HTML(css))
  ),
  tags$body(

    tags$h1('Stage 3 Step A: swap experiment results, in plain English'),
    tags$p(class = 'small',
      sprintf('Generated %s from output_dir=%s.', Sys.time(), output_dir)),

    tags$h2('TLDR'),
    tags$p(HTML('We added an amount term to the swap solver\'s objective. The result is that <strong>Step B (the per-cell rescale that was distorting top shares) is essentially eliminated</strong> — the typical cell now needs no rescale at all (median factor 1.000 vs 0.840 before). Top-1% NW share is +0.35pp from SCF (was +1.35pp). Counts are slightly worse on the headline ±5% metric (43.8% vs 47.9%), an acceptable trade for the much better amounts and top shares.')),

    tags$h2('What problem are we solving?'),
    tags$p(HTML('We have ~180k filer tax units from the PUF, plus ~41k DINA nonfiler units appended in <code>impute_nonfilers.R</code>, so Stage 3 processes ~221k rows in total. Each one needs to get a wealth profile (23 dollar variables: equities, bonds, primary home, debts, etc.). We do this by <em>donor matching</em>: pick an SCF household with similar income and demographics, copy its wealth profile.')),
    tags$p(HTML('The matching happens via a <em>random forest</em>. For each PUF tax unit, the forest gives a "leaf" of ~30–50 SCF donors that look like it on income/demographics. Step A picks one donor per record from that leaf.')),
    tags$p(HTML('We split the population into 16 cells (8 income buckets × 2 age groups) and 6 wealth categories (nw, equities, bonds, homes, other, debt), giving 96 buckets. For each bucket, SCF gives us:')),
    tags$ul(
      tags$li(tags$strong('count target'), ' — how many households (weighted) hold a positive amount of this category in this cell.'),
      tags$li(tags$strong('amount target'), ' — the total dollars (weighted) held in this category in this cell.')
    ),
    tags$p('Our donor picks should reproduce both per cell.'),

    tags$h2('What was wrong before this experiment'),
    tags$p(HTML('The swap solver only optimized counts. After it finished, the per-cell amounts could be 25–35% over SCF on aggregate — because the leaf donors with the right positivity pattern aren\'t necessarily the right magnitude.')),
    tags$p(HTML('We had a <strong>Step B</strong> that fixed the amounts by multiplying every record\'s wealth in (cell × category) by a corrective factor (typical factor ≈ 0.84). This works in the sense of "the aggregate matches SCF" but it has two costs you flagged:')),
    tags$ul(
      tags$li('It produces synthetic dollar values that no real SCF donor had.'),
      tags$li('It distorts the within-cell wealth distribution, which inflates measured top shares.')
    ),

    tags$h2('What this experiment changed'),
    tags$p(HTML('We added an <strong>amount term</strong> to the swap solver\'s objective. Now each proposed swap is evaluated against:')),
    tags$ul(
      tags$li('how it changes the counts across all 6 categories (as before)'),
      tags$li('plus how it changes the dollar amounts across the 5 amount-tracked categories (equities, bonds, homes, other, debt — nw is excluded because it\'s a sum of the others)')
    ),
    tags$p(HTML('The <code>amount_weight</code> knob controls how much the amount term weighs vs. the count term. We tested 0 (count-only, the old behavior), 0.5, 1, and 2. <code>amount_weight = 0.5</code> turned out to be the sweet spot.')),

    tags$h2('Headline numbers'),
    tags$h3('Match quality across the 96 buckets'),
    quality_tbl_html(match_quality_tbl),
    tags$p(class = 'small', 'Count-only is slightly better on count match (its only objective). Joint is dramatically better on amount match — what the experiment was about.'),

    tags$h3('Step B\'s rescale factor distribution'),
    tags$table(
      tags$thead(tags$tr(
        tags$th(''),
        tags$th('count-only'), tags$th('joint'))),
      tags$tbody(
        tags$tr(tags$td('median rescale factor'),
                tags$td(sprintf('%.3f', rf_co$median)),
                tags$td(sprintf('%.3f', rf_jt$median))),
        tags$tr(tags$td('median |log2(factor)| (0 = no rescale needed)'),
                tags$td(sprintf('%.3f', rf_co$median_log2)),
                tags$td(sprintf('%.3f', rf_jt$median_log2))),
        tags$tr(tags$td('cells with factor outside [0.5, 2.0]'),
                tags$td(rf_co$n_outside),
                tags$td(rf_jt$n_outside))
      )
    ),
    tags$p(HTML('A factor of 1.0 means swap got the amount exactly right and Step B is a no-op. The median jumped from <strong>0.840</strong> (Step B doing real work) to <strong>1.000</strong> (Step B is essentially idle).')),

    tags$h3('Top NW share, post-Step-B (what Tax-Simulator sees)'),
    tags$table(
      tags$thead(tags$tr(
        tags$th('share'), tags$th('SCF actual'),
        tags$th('count-only Δ'), tags$th('joint Δ'))),
      tags$tbody(lapply(seq_len(nrow(top_share_tbl)), function(i) {
        tags$tr(
          tags$td(top_share_tbl$share[i]),
          tags$td(top_share_tbl$scf[i]),
          tags$td(top_share_tbl$count_only_pp[i]),
          tags$td(top_share_tbl$joint_pp[i])
        )
      }))
    ),
    tags$p(HTML('Top-1 share gap to SCF dropped from <strong>+1.35pp</strong> to <strong>+0.35pp</strong>. This is what \'Step B is no longer distorting the within-cell distribution\' looks like in the actual output.')),

    tags$h2('Per-category bar charts'),
    intro_legend,
    cell_label_help,
    tags$div(class = 'note', HTML('<strong>How to read these:</strong> for each bucket of (income × age), the four bars show the SCF target, the raw forest output, the count-only swap output, and the joint swap output. <strong>The joint (green) bar should be at the same height as the SCF (gray) bar.</strong> When it is, the swap got that bucket right and Step B has no work to do.')),

    do.call(tagList, charts_html),

    tags$h2('What still doesn\'t work'),
    tags$ul(
      tags$li(HTML('<strong>Top-tail seniors</strong>: the cells <code>pct99.9to100/senior</code>, <code>pct80to90/senior</code>, etc. are still off in both counts and amounts. The leaf donor pool just doesn\'t contain enough of the right-shaped donors. This is a thin-sample problem on the SCF side — only ~570 senior rows in the top 0.1% income cell. Bigger leaves (mns=50 already used here) only partially help.')),
      tags$li(HTML('<strong>"Other" category</strong>: lumps 9 distinct wealth components into one aggregate. The solver has only one knob for an aggregate determined by 9 underlying values. The solver hits the aggregate amount target, but within-aggregate composition can be wrong. To fix this cleanly we would need to split "other" into a few sub-categories.')),
      tags$li(HTML('<strong>NW pre-rescale gap</strong>: nw isn\'t directly targeted (it\'s a derived linear combination of the other 5). It still drifts, indirectly, from the residual amount errors in the others. After Step B\'s now-tiny rescale, final nw amounts are essentially correct.'))
    ),

    tags$h2('Recommendation'),
    tags$p(HTML('Adopt the joint solver with <code>amount_weight = 0.5</code> as the new Step A. This gives:')),
    tags$ul(
      tags$li('Count match within ±5% of SCF in 44% of buckets (was 48% under count-only — a 4pp drop, acceptable).'),
      tags$li('Amount match within ±5% of SCF in 62% of buckets (was 12% — five-fold improvement).'),
      tags$li('Step B median rescale factor 1.000 (was 0.840) — Step B is essentially a sanity check, not a calibrator.'),
      tags$li('Top-1 NW share gap +0.35pp (was +1.35pp) — well inside the original ≤+1.5pp target.'),
      tags$li('Production output is real donor rows with negligible corrective scaling. The "ugly" data manipulation you were worried about is essentially gone.')
    ),
    tags$p(class = 'small', 'See docs/swap_vs_tilt_experiment.md for the full experimental record including all the variants we tested before landing here.')
  )
)


#--- Write -----------------------------------------------------------------

html_path = 'docs/swap_results.html'
write(as.character(html), html_path)
cat(sprintf('\nWrote %s\n', html_path))

cat('\nDone.\n')
