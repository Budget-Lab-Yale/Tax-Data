#--------------------------------------
# ingest_dfa.R
#
# Pulls the Federal Reserve's
# Distributional Financial Accounts
# (DFA) release bundle as a zip, extracts
# all per-slice CSVs (wealth / income /
# age / race / education / generation
# × levels / shares × aggregate /
# detail), and writes a tidy long-format
# snapshot to resources/dfa/dfa_totals.csv.
#
# Source:
#   https://www.federalreserve.gov/
#     releases/z1/dataviz/download/zips/
#     dfa.zip
#
# Coverage: quarterly 1989Q3 through
# the most recent DFA release (currently
# 2025Q4).
#
# Tidy output schema (dfa_totals.csv):
#   slice       wealth | income | age | race | education | generation
#   variant     aggregate | detail
#   year        integer
#   quarter     integer
#   bucket      e.g. "pct99to100" (income), "TopPt1" (wealth), "40_54" (age)
#   category    DFA financial category (e.g. "Deposits", "Corporate equities
#               and mutual fund shares")
#   dollars     numeric, US dollars (converted from the file's native
#               millions)
#
# Usage:
#   Rscript src/eda/ingest_dfa.R
#--------------------------------------

suppressMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

DFA_URL = 'https://www.federalreserve.gov/releases/z1/dataviz/download/zips/dfa.zip'
zip_path = 'resources/dfa/dfa.zip'
extract_dir = 'resources/dfa/raw'

if (!file.exists(zip_path)) {
  cat(sprintf('Downloading %s ...\n', DFA_URL))
  download.file(DFA_URL, zip_path, mode = 'wb')
} else {
  cat(sprintf('Using cached zip at %s (delete to re-fetch)\n', zip_path))
}

dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)
unzip(zip_path, exdir = extract_dir)
cat(sprintf('Extracted %d files to %s\n',
            length(list.files(extract_dir)), extract_dir))

# Slices we care about (levels only — dollar-value series, not shares).
# Each slice has an aggregate variant (fewer categories) and a detail
# variant (20-ish finer categories, closer to our 23-var schema).
# File naming note: Fed labels the wealth-percentile slice as "networth"
# on disk but "wealth" on the landing page.
slices = c('networth', 'income', 'age', 'race', 'education', 'generation')

# Non-financial columns to drop: bucket metadata rather than dollar series.
meta_categories = c('Household count',
                    'Minimum Income Cutoff',
                    'Maximum Income Cutoff')

parse_one = function(slice, variant) {
  file_name = if (variant == 'aggregate')
    sprintf('dfa-%s-levels.csv', slice)
  else
    sprintf('dfa-%s-levels-detail.csv', slice)

  path = file.path(extract_dir, file_name)
  if (!file.exists(path)) {
    cat(sprintf('  skip %s (not found)\n', file_name))
    return(NULL)
  }

  df = read_csv(path, show_col_types = FALSE)
  df %>%
    pivot_longer(cols      = -c(Date, Category),
                 names_to  = 'category',
                 values_to = 'dollars_m') %>%
    filter(!(category %in% meta_categories)) %>%
    mutate(slice    = slice,
           variant  = variant,
           year     = as.integer(substr(Date, 1, 4)),
           quarter  = as.integer(substr(Date, 7, 7)),
           bucket   = Category,
           dollars  = dollars_m * 1e6) %>%
    select(slice, variant, year, quarter, bucket, category, dollars)
}

cat('\nparsing DFA files:\n')
combos = expand.grid(slice = slices,
                     variant = c('aggregate', 'detail'),
                     stringsAsFactors = FALSE)
all_dfa = pmap_dfr(combos, function(slice, variant) {
  cat(sprintf('  %s × %s\n', slice, variant))
  parse_one(slice, variant)
})

all_dfa = all_dfa %>%
  arrange(slice, variant, year, quarter, bucket, category)

write_csv(all_dfa, 'resources/dfa/dfa_totals.csv')
cat(sprintf('\nwrote resources/dfa/dfa_totals.csv: %d rows\n', nrow(all_dfa)))

# Sanity: 2022Q4 income-percentile × detail categories
cat('\n2022Q4 income-bucket × detail (T):\n')
all_dfa %>%
  filter(year == 2022, quarter == 4,
         slice == 'income', variant == 'detail') %>%
  mutate(T = round(dollars / 1e12, 3)) %>%
  select(bucket, category, T) %>%
  arrange(bucket, category) %>%
  print(n = 40)
