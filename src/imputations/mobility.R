#--------------------------------------
# mobility.R
#
# Imputes parent's income percentile
# during childhood using Chetty et al.
# intergenerational mobility matrix
#--------------------------------------


# Read intergenerational mobility parameters
mobility_matrix = 'resources/mobility_matrix.csv' %>%
  read_csv(show_col_types = F) %>%
  pivot_longer(cols            = -child_rank,
               names_to        = 'parent_rank',
               names_transform = as.integer,
               values_to       = 'pdf') %>%
  relocate(parent_rank, .before = everything()) %>%
  arrange(parent_rank, child_rank) %>%

  # Aggregate bottom 10 percent of children to deal with gaps around 0 income
  mutate(child_rank = pmax(10, child_rank)) %>%
  group_by(parent_rank, child_rank) %>%
  summarise(pdf = sum(pdf),
            .groups = 'drop')


tax_units %<>%

  # Derive income measure
  mutate(
    income = wages + txbl_int + div_ord + div_pref + state_ref +
             txbl_ira_dist + txbl_pens_dist + kg_lt + kg_st + other_gains +
             sole_prop + part_active + part_passive - part_active_loss -
             part_passive_loss - part_179 + scorp_active + scorp_passive -
             scorp_active_loss - scorp_passive_loss - scorp_179 + rent -
             rent_loss + estate - estate_loss + farm + ui + gross_ss + other_inc
  ) %>%

  # Add income rank
  mutate(
    child_rank = cut(
      x = income,
      breaks = wtd.quantile(
        x       = .$income,
        weights = .$weight,
        probs = 9:100/100
      ),
      labels = 10:100
    ) %>%
      as.character() %>%
      as.integer() %>%
      replace_na(100),
   group = ceiling(row_number() / 2000)
  )


# Expand to parent rank and sample, in chunks for memory reasons
parent_ranks = list()
for (g in unique(tax_units$group)) {
  parent_ranks[[g]] = tax_units %>%
    filter(group == g) %>%
    left_join(mobility_matrix, by = 'child_rank', relationship = 'many-to-many') %>%
    group_by(id) %>%
    sample_n(1, weight = pdf) %>%
    select(id, parent_rank)
}

tax_units %<>%
  left_join(parent_ranks %>%
              bind_rows(),
            by = 'id')
