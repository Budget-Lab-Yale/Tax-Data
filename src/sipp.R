library(data.table)
library(bit64)
library(magrittr)
library(tidyverse)
library(Hmisc)
library(dineq)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Define function to assign ranks for a given variable 
assign_ranks = function(values, weights, quantiles, pos_only = T) {
  
  # Add jitter to ensure unique breaks
  values = values + ((values != 0) * runif(n = length(values), min = -1, max = 1))
  
  # Subset data 
  values_subset  = values
  weights_subset = weights
  if (pos_only) {
    values_subset  = values[values > 5]  # 5 dollar assumed de-minimus reporting standard
    weights_subset = weights[values > 5]
  }
  
  # Calculate cutoffs
  cutoffs = c(wtd.quantile(values_subset, weights_subset, quantiles), Inf) 
  
  # Apply 
  cut(x = values, breaks = cutoffs, include.lowest = T, labels = quantiles) %>% 
    as.character() %>% 
    as.numeric() %>% 
    return()
}

build_annual_sipp = function(year, occ = F) {
  
  pu = file.path('/gpfs/gibbs/project/sarin/shared/raw_data/SIPP', year, paste0('pu',year,'.csv')) %>%
    fread(., sep = '|', select = c(
      'SSUID','PNUM','MONTHCODE','ERESIDENCEID','ERELRPE','SPANEL','SWAVE',
      
      'WPFINWGT',
      
      'ESEX','TAGE','TAGE_EHC','ERACE','EORIGIN','EEDUC', 'EDEPCLM', 'EMS', 'EFSTATUS',
      
      'TJB1_TXAMT', 'TJB1_MSUM', 'TJB1_OCC', 'TJB1_IND', 'AJB1_TXAMT', 'EJB1_TYPPAY3',
      'TJB2_TXAMT', 'TJB2_MSUM', 'TJB2_OCC', 'TJB2_IND', 'AJB2_TXAMT', 'EJB2_TYPPAY3',
      'TJB3_TXAMT', 'TJB3_MSUM', 'TJB3_OCC', 'TJB3_IND', 'AJB3_TXAMT', 'EJB3_TYPPAY3',
      'TJB4_TXAMT', 'TJB4_MSUM', 'TJB4_OCC', 'TJB4_IND', 'AJB4_TXAMT', 'EJB4_TYPPAY3',
      
      'TPTOTINC'
    ))
  
  names(pu) = toupper(names(pu))

  data = 
    pu %>%
    rename(
      INC  = TPTOTINC,
      AJB1 = AJB1_TXAMT,
      AJB2 = AJB2_TXAMT,
      AJB3 = AJB3_TXAMT,
      AJB4 = AJB4_TXAMT
    ) %>%
    mutate(
      u_ID = paste0(SSUID, PNUM),
      TIPS = rowSums(select(., contains("TXAMT")), na.rm = T),
      tip_jobs = rowSums(ifelse(!is.na(select(., contains("TXAMT"))), 1, 0)),
      jobs = rowSums(ifelse(!is.na(select(., contains("OCC"))), 1, 0)),
      t_flag = TIPS > 0,
      i_flag = rowSums(ifelse(select(., contains("AJB")) > 1, 1, 0)),
      i_flag = i_flag > 0,
      EARN_SUM = rowSums(select(., contains("MSUM")), na.rm = T),
      TIPS_pct = TIPS / EARN_SUM,
      across(contains('TYPPAY'), ~ .x %% 2)
    ) %>%
    group_by(SSUID) %>%
    mutate(
      child_count = sum(if_else(TAGE < 18 & MONTHCODE == 12, 1, 0)),
      young_child_count = sum(if_else(TAGE < 6 & MONTHCODE == 12, 1, 0))
    ) %>%
    ungroup()
  
  an_data = data %>%
    group_by(u_ID) %>%
    reframe(
      sex = getmode(ESEX),
      age = max(TAGE),
      marriage = getmode(EMS),
      race = getmode(ERACE),
      educ = max(EEDUC),
      n_dep = getmode(child_count),
      n_young_dep = getmode(young_child_count),
      is_dep = getmode(EDEPCLM),
      is_dep = if_else(is.na(is_dep) | is_dep == 2, F, T),
      
      
      weight = sum(WPFINWGT) / 12,
      inc_tot = sum(INC) / 12,
      inc_tip = sum(TIPS),
      tipped = sum(t_flag) > 0,
      inc_earn = sum(EARN_SUM),
      tips_pct = inc_tip / inc_earn,
      tips_imputed = sum(i_flag) > 0
    )  %>%
    filter(age > 17) 

  if(occ) {
    an_data %<>%
      mutate(
        n_jobs = length(unique(TJB1_OCC, incomparables = T)),
        occ1_t = sum(EJB1_TYPPAY3, na.rm = T) > 0,
        occ1_1 = unique(TJB1_OCC)[1],
        occ1_2 = if_else(n_jobs > 1, unique(TJB1_OCC)[2], NA),
        occ1_3 = if_else(n_jobs > 2, unique(TJB1_OCC)[3], NA),
        occ1_1 = if_else(is.na(occ1_1) & !(is.na(occ1_2)), occ1_2, occ1_1),
        occ1_2 = if_else(occ1_1 == occ1_2, NA, occ1_2),
        
        n_inds = length(unique(TJB1_IND, incomparables = T)),
        ind1_t = sum(EJB1_TYPPAY3, na.rm = T) > 0,
        ind1_1 = unique(TJB1_IND)[1],
        ind1_2 = if_else(n_inds > 1, unique(TJB1_OCC)[2], NA),
        ind1_3 = if_else(n_inds > 2, unique(TJB1_OCC)[3], NA),
        ind1_1 = if_else(is.na(ind1_1) & !(is.na(ind1_2)), ind1_2, ind1_1),
        ind1_2 = if_else(ind1_1 == ind1_2, NA, ind1_2),
        
        n_jobs = length(unique(TJB2_OCC, incomparables = T)),
        occ2_t = sum(EJB2_TYPPAY3, na.rm = T) > 0,
        occ2_1 = unique(TJB2_OCC)[1],
        occ2_2 = if_else(n_jobs > 1, unique(TJB2_OCC)[2], NA),
        occ2_3 = if_else(n_jobs > 2, unique(TJB2_OCC)[3], NA),
        occ2_1 = if_else(is.na(occ2_1) & !(is.na(occ2_2)), occ2_2, occ2_1),
        occ2_2 = if_else(occ2_1 == occ2_2, NA, occ2_2),
        
        n_inds = length(unique(TJB2_IND, incomparables = T)),
        ind2_t = sum(EJB2_TYPPAY3, na.rm = T) > 0,
        ind2_1 = unique(TJB2_IND)[1],
        ind2_2 = if_else(n_inds > 1, unique(TJB2_OCC)[2], NA),
        ind2_3 = if_else(n_inds > 2, unique(TJB2_OCC)[3], NA),
        ind2_1 = if_else(is.na(ind2_1) & !(is.na(ind2_2)), ind2_2, ind2_1),
        ind2_2 = if_else(ind2_1 == ind2_2, NA, ind2_2),
        
        n_jobs = length(unique(TJB3_OCC, incomparables = T)),
        occ3_t = sum(EJB3_TYPPAY3, na.rm = T) > 0,
        occ3_1 = unique(TJB3_OCC)[1],
        occ3_2 = if_else(n_jobs > 1, unique(TJB3_OCC)[2], NA),
        occ3_3 = if_else(n_jobs > 2, unique(TJB3_OCC)[3], NA),
        occ3_1 = if_else(is.na(occ3_1) & !(is.na(occ3_2)), occ3_2, occ3_1),
        occ3_2 = if_else(occ3_1 == occ3_2, NA, occ3_2),
        
        n_inds = length(unique(TJB3_IND, incomparables = T)),
        ind3_t = sum(EJB3_TYPPAY3, na.rm = T) > 0,
        ind3_1 = unique(TJB3_IND)[1],
        ind3_2 = if_else(n_inds > 1, unique(TJB3_OCC)[2], NA),
        ind3_3 = if_else(n_inds > 2, unique(TJB3_OCC)[3], NA),
        ind3_1 = if_else(is.na(ind3_1) & !(is.na(ind3_2)), ind3_2, ind3_1),
        ind3_2 = if_else(ind3_1 == ind3_2, NA, ind3_2),
        
        n_jobs = length(unique(TJB4_OCC, incomparables = T)),
        occ4_t = sum(EJB4_TYPPAY3, na.rm = T) > 0,
        occ4_1 = unique(TJB4_OCC)[1],
        occ4_2 = if_else(n_jobs > 1, unique(TJB4_OCC)[2], NA),
        occ4_3 = if_else(n_jobs > 2, unique(TJB4_OCC)[3], NA),
        occ4_1 = if_else(is.na(occ4_1) & !(is.na(occ4_2)), occ4_2, occ4_1),
        occ4_2 = if_else(occ4_1 == occ4_2, NA, occ4_2),
        
        n_inds = length(unique(TJB4_IND, incomparables = T)),
        ind4_t = sum(EJB4_TYPPAY3, na.rm = T) > 0,
        ind4_1 = unique(TJB4_IND)[1],
        ind4_2 = if_else(n_inds > 1, unique(TJB4_OCC)[2], NA),
        ind4_3 = if_else(n_inds > 2, unique(TJB4_OCC)[3], NA),
        ind4_1 = if_else(is.na(ind4_1) & !(is.na(ind4_2)), ind4_2, ind4_1),
        ind4_2 = if_else(ind4_1 == ind4_2, NA, ind4_2),
        
        primary_occ = occ1_1,
        primary_occ_tipped = occ1_t,
        primary_ind = ind1_1,
        
        primary_occ = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ2_t), occ2_1, primary_occ),
        primary_occ_tipped = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ2_t), occ2_t, primary_occ_tipped),
        primary_ind = if_else(is.na(primary_ind) | (!primary_occ_tipped & occ2_t), ind2_1, primary_ind),
        
        primary_occ = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ3_t), occ3_1, primary_occ),
        primary_occ_tipped = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ3_t), occ3_t, primary_occ_tipped),
        primary_ind = if_else(is.na(primary_ind) | (!primary_occ_tipped & occ3_t), ind3_1, primary_ind),
        
        primary_occ = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ4_t), occ4_1, primary_occ),
        primary_occ_tipped = if_else(is.na(primary_occ) | (!primary_occ_tipped & occ4_t), occ4_t, primary_occ_tipped),
        primary_ind = if_else(is.na(primary_ind) | (!primary_occ_tipped & occ4_t), ind3_1, primary_ind),
      ) %>%
      select(!contains(c('_1', '_2', '_3')) & !ends_with('_t') & !n_jobs)
  }
  
  
  an_data %>%
    write_csv(., file.path('/gpfs/gibbs/project/sarin/shared/raw_data/SIPP', year, 'tip_data.csv'))
  
  
  print(paste0('SIPP Year ', year, ' annualized'))
  
}

build_annual_sipp(2019)


build_tip_panel = function(year, write = F) {
  
  an_data = file.path('/gpfs/gibbs/project/sarin/shared/raw_data/SIPP', year, 'tip_data.csv') %>%
    fread() %>%
    tibble()
  
  employed = an_data %>% filter(inc_earn > 0 & !is.na(inc_earn)) %>%
    mutate(
      marriage = if_else(marriage < 3 & !is.na(marriage), T, F),
      earn_dec = cut(
        inc_earn,
        breaks = wtd.quantile(
          inc_earn,
          weights = weight,
          probs = seq(0,1,.1)
        ),
        labels = 1:10,
        include.lowest = T
      )
    )
  
  base_panel = employed %>%
    group_by(earn_dec) %>%
    reframe(
      count = sum(weight),
      n_rec = sum(1 * tipped),
      t_count = sum(weight * tipped),
      
      p = (t_count / count),
      mean = wtd.mean(tips_pct, (weight * tipped))
    ) %>%
    select(earn_dec, t_count, n_rec, p, mean) %>%
    left_join(
      employed %>%
        filter(tipped) %>%
        group_by(earn_dec) %>%
        reframe(
          p = paste0('p',seq(0, 100, 10)),
          val = wtd.quantile(tips_pct, weight, seq(0, 1, .1)),
        ) %>%
        pivot_wider(
          names_from = p, values_from = val
        )
    ) %>%
    mutate(earn_dec = factor(earn_dec)) %>%
    rbind(.,
          employed %>%
            filter(tipped) %>%
            reframe(
              earn_dec = factor('total'),
              t_count = sum(weight),
              n_rec = n(),
              p = t_count / sum(employed$weight),
              mean = wtd.mean(tips_pct, weight),
              val = wtd.quantile(tips_pct, weight, seq(0,1,.1)),
              p_ = paste0('p',seq(0, 100, 10))
            ) %>%
            pivot_wider(
              names_from = p_, values_from = val
            ) 
    ) %>%
    rename(
      weighted_n = t_count
    ) 
  
  if(write){
     write_csv(base_panel, 
               file.path('/gpfs/gibbs/project/sarin/shared/model_data/SIPP', year, 'tip_panel.csv')
               )
  }
  
  return(base_panel)
}

base_panel = build_tip_panel(2019)

apply_tip_probs = function(tax_units, panel, filter = NA, prob = "p") {
  persons = tax_units %>%
    mutate(
      married.1      = as.integer(filing_status == 2), 
      n_kids.1       = as.integer(!is.na(dep_age1) & dep_age1 <= 24) + 
        as.integer(!is.na(dep_age2) & dep_age2 <= 24) + 
        as.integer(!is.na(dep_age3) & dep_age3 <= 24), 
      n_kids_u5.1    = as.integer(!is.na(dep_age1) & dep_age1 < 5) + 
        as.integer(!is.na(dep_age2) & dep_age2 < 5) + 
        as.integer(!is.na(dep_age3) & dep_age3 < 5),
      n_kids_0.1     = as.integer(!is.na(dep_age1) & dep_age1 == 0) + 
        as.integer(!is.na(dep_age2) & dep_age2 == 0) + 
        as.integer(!is.na(dep_age3) & dep_age3 == 0), 
      n_workers.1    = as.integer(wages1 > 0) + as.integer(wages2 > 0),
      tu_other_inc.1 = txbl_int + exempt_int + div_ord + div_pref + sole_prop + 
        txbl_ira_dist + gross_pens_dist + part_active - part_active_loss + 
        part_passive - part_passive_loss + scorp_active - scorp_active_loss +
        scorp_passive - scorp_passive_loss + rent - rent_loss +
        estate - estate_loss + farm + ui,
    ) %>%
    select(
      year, tax_unit_id = id, weight.1 = weight, married.1, age1, age2, 
      starts_with('n_kids'), starts_with('dep_age'), n_workers.1, filing_status,
      wages1, wages2, tu_wages.1 = wages, tu_ss.1 = gross_ss, tu_other_inc.1
    ) %>%
    
    # Pivot long in person
    pivot_longer(
      cols            = c(age1, age2), 
      names_prefix    = 'age', 
      names_transform = as.integer, 
      names_to        = 'person_id', 
      values_to       = 'age.1'
    ) %>% 
    filter(!is.na(age.1)) %>% 
    
    # Assign ranks for income variables
    mutate(
      wages.1 = if_else(person_id == 1, wages1, wages2), 
      across(
        .cols = c(wages.1, tu_wages.1, tu_ss.1, tu_other_inc.1), 
        .fns  = ~ assign_ranks(
          values    = ., 
          weights   = weight.1, 
          quantiles = seq(0,.9,.1) #c(seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001))
        ) %>% replace_na(-1), 
        .names = 'q.{col}'
      ),
      earn_dec = if_else(q.wages.1 == -1, -1, (q.wages.1 * 10)+1)
    ) %>% 
    select(-wages1, -wages2)
  
  if(!is.na(filter)) {
    
    if(filter==2) {
      persons %<>%
        filter(filing_status == filter)  
    } else {
      persons %<>%
        filter(filing_status != 2)
    }
  }
  
  1:10 %>%
    map(.f = ~ persons %>%
          filter(earn_dec == .x) %>%
          sample_frac(.,
                      size = unlist(panel[.x, prob])
          ) %>%
          mutate(
            tips_pct = sample(
              c(unname(unlist(panel[.x, c(paste0('p',seq(0,100,10)))]))),
              n(), 
              replace = T
            ),
            wages_tip = wages.1 * tips_pct
          ) %>%
          select(tax_unit_id, person_id, wages_tip)
    ) %>%
    bind_rows() %>%
    group_by(tax_unit_id) %>%
    pivot_wider(
      names_from = person_id,
      names_prefix = 'tips.',
      values_from = wages_tip
    )  %>%
    return()

}

assign_tips = function(tax_units, panel) {
  
  # ALTERNATIVELY, READ IN PANEL HERE SINCE IT DOESN'T CHANGE IN THIS FUNCTION
    
  dist_1 = tax_units %>%
    left_join(., apply_tip_probs(tax_units, panel), by = join_by(id == tax_unit_id)) %>%

  # dist_1 = apply_tip_probs(tax_units, panel) %>%
  #   left_join(tax_units, ., by = join_by(id == tax_unit_id)) %>%
    mutate(
      tips.1 = if_else(is.na(tips.1), 0, tips.1),
      tips.2 = if_else(is.na(tips.2), 0, tips.2),
      tips = tips.1 + tips.2
    ) %>%
    filter(wages > 0) %>%
    mutate(married = filing_status == 2,
           tipped = tips > 0) %>%
    group_by(married, tipped) %>%
    reframe(
      count = sum(weight)
    ) %>%
    group_by(married) %>%
    mutate(pct = count / sum(count)) %>%
    mutate(
      scalar = (
        (((married == 1) * .0172 ) + ((married == 0) * .0641))  / pct 
      )
    ) %>% ungroup() %>%
    filter(tipped) %>%
    print()
  
  
  panel2 = panel %>%
    mutate(
      p_m = p * unname(unlist(dist_1 %>% filter(married) %>% select(scalar))),
      p_s = p * unname(unlist(dist_1 %>% filter(!married) %>% select(scalar))),
    )
  
  print(panel2[,c("p","p_m","p_s")])
  
  tipped = apply_tip_probs(tax_units, panel2, filter = 2, prob = "p_m") %>%
    bind_rows(
      apply_tip_probs(tax_units, panel2, filter = 1, prob = "p_s")
    )
  
  tax_units %>%
    left_join(., tipped, by = join_by(id == tax_unit_id)) %>%
    mutate(
      tips.1 = if_else(is.na(tips.1), 0, tips.1),
      tips.2 = if_else(is.na(tips.2), 0, tips.2),
      tips = tips.1 + tips.2
    ) %>%
    return()
  
}

tax_units = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Data/v1/2024070817/baseline/tax_units_2018.csv') 

check = assign_tips(tax_units, base_panel)

check %>%
  filter(wages > 0) %>%
  mutate(
    tipped = tips > 0,
    married = filing_status == 2
  ) %>%
  group_by(married, tipped) %>%
  reframe(
    count = sum(weight)
  ) %>%
  group_by(married) %>%
  mutate(pct = count / sum(count))


  