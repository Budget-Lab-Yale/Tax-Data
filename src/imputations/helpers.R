#--------------------------------------
# helpers.R
#
# Shared utility functions for
# imputation scripts
#--------------------------------------


#' Train or load a quantile regression forest model
#'
#' If estimate_models is TRUE, trains a new QRF and caches it.
#' Otherwise, loads the cached model from disk.
#'
#' @param name     Model name (used for cache file path: resources/cache/qrf/{name}.rds)
#' @param x        Training features (data.frame or matrix)
#' @param y        Response variable
#' @param weights  Observation weights
#' @param mtry     Number of variables sampled at each split
#' @param nodesize Minimum node size
#' @return         A quantregForest object
train_or_load_qrf = function(name, x = NULL, y = NULL, weights = NULL,
                              mtry = 3, nodesize = 5) {

  cache_path = paste0('resources/cache/qrf/', name, '.rds')

  if (estimate_models) {
    qrf = quantregForest(
      x        = x,
      y        = y,
      nthreads = parallel::detectCores(),
      weights  = weights,
      mtry     = mtry,
      nodesize = nodesize
    )
    write_rds(qrf, cache_path)
  } else {
    qrf = read_rds(cache_path)
  }

  qrf
}


#' Compute weighted percentile bins
#'
#' Bins a numeric vector into percentile groups using weighted quantiles.
#' Values <= 0 are assigned percentile 0. NAs from binning are replaced with 0.
#'
#' @param x      Numeric vector to bin
#' @param weight Weight vector (same length as x)
#' @return       Integer vector of percentile bins (0-100)
compute_percentile = function(x, weight) {
  cut(
    x      = x,
    breaks = wtd.quantile(x[x > 0], weight[x > 0], 0:100 / 100),
    labels = 1:100
  ) %>%
    as.character() %>%
    as.integer() %>%
    replace_na(0)
}


#' Pivot spouse-level wage and age columns to long form
#'
#' Transforms a dataframe with wages1/wages2 and age1/age2 columns into
#' long form with one row per earner, filtering to those with positive wages.
#'
#' @param df  Dataframe containing id, wages1, wages2, age1, age2
#' @return    Long-form dataframe with columns: id, ..., index, wages, age
pivot_to_spouses = function(df) {
  df %>%
    pivot_longer(
      cols            = c(wages1, wages2),
      names_prefix    = 'wages',
      names_transform = as.integer,
      names_to        = 'index',
      values_to       = 'wages'
    ) %>%
    mutate(age = if_else(index == 1, age1, age2)) %>%
    select(-age1, -age2) %>%
    filter(wages > 0)
}
