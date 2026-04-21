#--------------------------------------
# helpers.R
#
# Shared utility functions for
# imputation scripts
#--------------------------------------


#' Resolve the number of CPU threads to use.
#'
#' On SLURM, parallel::detectCores() returns the node's physical core count,
#' not the allocation — which oversubscribes and wastes time on context
#' switching. Prefer SLURM_CPUS_PER_TASK when set; off-cluster, fall back to
#' parallel::detectCores().
n_threads = function() {
  n = Sys.getenv('SLURM_CPUS_PER_TASK')
  if (nzchar(n)) as.integer(n) else parallel::detectCores()
}


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
      nthreads = n_threads(),
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
  # findInterval tolerates tied weighted-quantile breaks (common in skewed
  # distributions with concentrated values or topcoding), unlike cut() which
  # errors on duplicate breaks. Matches the construction in src/cex.R.
  breaks = wtd.quantile(x[x > 0], weight[x > 0], seq(0.01, 0.99, 0.01))
  pct = findInterval(x, breaks) + 1L
  pct = pmin(pct, 100L)
  pct[x <= 0 | is.na(x)] = 0L
  as.integer(pct)
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


#' Train or load a ranger quantile regression forest model
#'
#' If estimate_models is TRUE, trains a new ranger model and caches it.
#' Otherwise, loads the cached model from disk.
#'
#' @param name           Model name (used for cache file path)
#' @param formula        Model formula
#' @param data           Training data
#' @param case_weights   Observation weights
#' @param mtry           Number of variables sampled at each split
#' @param min_node_size  Minimum node size
#' @param num_trees      Number of trees
#' @return               A ranger object with quantreg = TRUE
train_or_load_ranger = function(name, formula, data, case_weights = NULL,
                                 mtry = 3, min_node_size = 5, num_trees = 500) {
  cache_path = paste0('resources/cache/qrf/', name, '.rds')
  if (estimate_models) {
    rf = ranger(formula, data = data, case.weights = case_weights,
                quantreg = TRUE, num.trees = num_trees, mtry = mtry,
                min.node.size = min_node_size, num.threads = n_threads())
    write_rds(rf, cache_path)
  } else {
    rf = read_rds(cache_path)
  }
  rf
}


#' Draw a random quantile prediction from a ranger model
#'
#' For each observation, predicts the full quantile function and draws
#' one random quantile to produce stochastic imputations.
#'
#' @param model   A ranger object trained with quantreg = TRUE
#' @param newdata Prediction data
#' @return        Numeric vector of predictions (one per row of newdata)
predict_ranger_draw = function(model, newdata) {
  grid = seq(0.01, 0.99, 0.01)
  pred = predict(model, data = newdata, type = 'quantiles', quantiles = grid)$predictions
  sapply(1:nrow(newdata), function(i) pred[i, sample(length(grid), 1)])
}


#' Train or load a cached DRF (Distributional Random Forest)
#'
#' Fits a DRF model on a multivariate response (e.g., expenditure share vectors)
#' and caches to disk. Uses FourierMMD splitting rule from the drf package.
#'
#' @param name           Cache name (saved to resources/cache/qrf/{name}.rds)
#' @param X              Matrix of features
#' @param Y              Matrix of responses (e.g., share vectors)
#' @param num.trees      Number of trees (default 500)
#' @param splitting.rule Splitting rule (default "FourierMMD")
#' @param min.node.size  Minimum node size (default 20)
#' @param honesty        Use honesty splitting (default TRUE)
#' @return               A drf object
train_or_load_drf = function(name, X, Y, num.trees = 500,
                              splitting.rule = "FourierMMD",
                              mtry = ncol(X),
                              min.node.size = 20, honesty = TRUE,
                              response.scaling = FALSE) {
  cache_path = paste0('resources/cache/qrf/', name, '.rds')
  if (estimate_models) {
    model = drf::drf(X = X, Y = Y, num.trees = num.trees,
                     splitting.rule = splitting.rule,
                     mtry = mtry,
                     min.node.size = min.node.size,
                     honesty = honesty,
                     response.scaling = response.scaling,
                     num.threads = n_threads())
    write_rds(model, cache_path)
  } else {
    model = read_rds(cache_path)
  }
  model
}
