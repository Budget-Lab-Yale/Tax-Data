#--------------------------------------
# ingest_dfa.R
#
# One-time / vintage-bump script to fetch
# the Federal Reserve's Distributional
# Financial Accounts (DFA) networth-levels
# table, aggregate the 5 wealth-percentile
# buckets into national totals per
# (quarter × category), and write a tidy
# snapshot to resources/dfa/dfa_totals.csv
# for downstream wealth calibration.
#
# Source: Federal Reserve Board, DFA release.
# Download URL (2026-04 vintage):
#   https://www.federalreserve.gov/
#     releases/z1/dataviz/download/
#     dfa-networth-levels.csv
#
# Schema (13 cols + Date + Category):
#   Date      YYYY:QN
#   Category  TopPt1 | RemainingTop1 | Next9 | Next40 | Bottom50
#   Net worth, Assets, Real estate, Consumer durables,
#   Corporate equities and mutual fund shares,
#   DB pension entitlements, DC pension entitlements,
#   Private businesses, Other assets,
#   Liabilities, Home mortgages, Consumer credit,
#   Other liabilities
#   (units: millions of dollars)
#
# See resources/dfa/dfa_schema_crosswalk.csv for the
# 23-dim Y-schema → DFA-11 mapping (many Y-vars collapse
# into one DFA bucket).
#
# Usage:
#   Rscript src/eda/ingest_dfa.R
#--------------------------------------

suppressMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
})

DFA_URL = 'https://www.federalreserve.gov/releases/z1/dataviz/download/dfa-networth-levels.csv'
raw_path = 'resources/dfa/dfa_raw_networth_levels.csv'

if (!file.exists(raw_path)) {
  cat(sprintf('Downloading DFA from %s ...\n', DFA_URL))
  download.file(DFA_URL, raw_path, mode = 'wb')
} else {
  cat(sprintf('Using cached raw download at %s (delete to re-fetch)\n', raw_path))
}

raw = read_csv(raw_path, show_col_types = FALSE)
stopifnot(all(c('Date', 'Category') %in% names(raw)))

# Sum across the 5 wealth-percentile buckets per (Date, category) → national.
nat = raw %>%
  select(-Category) %>%
  group_by(Date) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  mutate(year    = as.integer(substr(Date, 1, 4)),
         quarter = as.integer(substr(Date, 7, 7))) %>%
  select(-Date)

dfa_totals = nat %>%
  pivot_longer(cols = -c(year, quarter),
               names_to = 'dfa_category',
               values_to = 'dollars_m') %>%
  mutate(dollars = dollars_m * 1e6) %>%
  select(year, quarter, dfa_category, dollars) %>%
  arrange(year, quarter, dfa_category)

cat(sprintf('DFA totals: %d rows, %d–%d quarters\n',
            nrow(dfa_totals),
            min(dfa_totals$year), max(dfa_totals$year)))

write_csv(dfa_totals, 'resources/dfa/dfa_totals.csv')
cat('wrote resources/dfa/dfa_totals.csv\n')

# Sanity check: print 2022 Q4 aggregates in $T.
cat('\n2022 Q4 national aggregate (T):\n')
dfa_totals %>%
  filter(year == 2022, quarter == 4) %>%
  mutate(T = round(dollars / 1e12, 2)) %>%
  select(dfa_category, T) %>%
  print(n = Inf)
