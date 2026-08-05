#--------------------------------------
# earnings_split.R
#
# Imputes wage split between spouses
# using Saez tabulations, and splits
# self-employment income proportionally
#--------------------------------------


# Get and impute detail for Saez's tabulations of wage split tabulations
wage_split_cdf = read_csv('./resources/wagesplit.csv') %>%
  filter(year == 2014) %>%

  # Assign midpoint as earnings share
  select(wage_pctile  = wagepercentilelower,
         `0`          = share_female_wages_0th_instant,
         `0.025`      = share_female_wages_0th_5th,
         `0.15`       = share_female_wages_5th_25th,
         `0.375`      = share_female_wages_25th_50th,
         `0.675`      = share_female_wages_50th_75th,
         `0.875`      = share_female_wages_75th_99_99th,
         `1`          = share_female_wages_99_99th_100th) %>%
  pivot_longer(cols      = -wage_pctile,
               names_to  = 'secondary_share',
               values_to = 'p') %>%
  group_by(wage_pctile) %>%
  mutate(secondary_share = as.numeric(secondary_share),
         p               = cumsum(p))


# Calculate wage earnings percentile on PUF
pctiles = unique(wage_split_cdf$wage_pctile)
wage_thresholds = c(wtd.quantile(x       = tax_units$wages[tax_units$wages > 0],
                                 weights = tax_units$weight[tax_units$wages > 0],
                                 probs   = pctiles),
                    1e99)

wage_primary_share = tax_units %>%

  # Assign wage percentile
  mutate(wage_pctile = cut(x              = wages,
                           breaks         = wage_thresholds,
                           include.lowest = T,
                           right          = F,
                           labels         = pctiles) %>%
           as.character() %>%
           as.numeric() %>%
           replace_na(0)) %>%

  mutate(draw = runif(nrow(.), max = 0.999)) %>%  # due to rounding, some of the PDFs do not sum to exactly 1
  left_join(wage_split_cdf,
            by           = 'wage_pctile',
            relationship = 'many-to-many') %>%
  filter(draw <= p) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(wage_primary_share = 1 - secondary_share) %>%
  select(id, wage_primary_share)


# Add imputations to dataframe
tax_units %<>%
  left_join(wage_primary_share, by = 'id') %>%
  mutate(

    # Override imputation for records with supplemental demographic info available
    wage_primary_share = case_when(EARNSPLIT == 1 ~ 1,
                                   EARNSPLIT == 2 ~ 0.5,
                                   EARNSPLIT == 3 ~ 0,
                                   T              ~ wage_primary_share),

    # Set to 100% for all non-joint returns
    wage_primary_share = if_else(filing_status != 2, 1, wage_primary_share),

    # Set wages
    wages1 = wages * wage_primary_share,
    wages2 = wages * (1 - wage_primary_share)
  )


# Impute self-employment component split in proportion to total split
tax_units %<>%
  mutate(
    se_primary_share = if_else(E30400 + E30500 == 0,
                               1,
                               E30400 / (E30400 + E30500)),

    sole_prop1 = sole_prop * se_primary_share,
    sole_prop2 = sole_prop * (1 - se_primary_share),

    farm1 = farm * se_primary_share,
    farm2 = farm * (1 - se_primary_share),

    part_se1 = part_se * se_primary_share,
    part_se2 = part_se * (1 - se_primary_share)
  )
