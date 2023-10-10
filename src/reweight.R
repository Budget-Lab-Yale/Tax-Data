#------------------------------------------------------------------------------
# reweight.R 
#
# Contains functions to reweight PUF such that specified target values are hit
#------------------------------------------------------------------------------


reweight_lp = function(puf, targets, e = NULL, e_runs = 10) {
  
  #----------------------------------------------------------------------------
  # Adjusts weights using Linear Programming such that PUF records match 
  # Statistics of Income totals for a given year.
  # 
  # Parameters:
  #   - puf (df)     : tibble of tax units, processed
  #   - targets (df) : tibble of target parameter totals, processed
  #   - e (dbl)      : episilon value; if NULL, find optimal value using e_runs
  #   - e_runs (int) : number of iterations to attempt to find ideal maximum
  #                    deviation from observed weights
  # 
  # Returns: if solver finds a valid solution, vector of weight rescaling 
  #          factors; if not, returns NULL
  #----------------------------------------------------------------------------  
  
  # Construct left hand side of constraint equations
  lhs = build_lhs(puf, targets)
  
  # Run Linear Programming solver
  return(run_lp(lhs, targets, e, e_runs))
}



build_lhs = function(puf, targets) {
  
  #----------------------------------------------------------------------------
  # Constructs left hand side of constraint equations for the LP Solver
  # 
  # Parameters:
  #   - puf (df)       : tibble of tax units, processed, weights scaled to 
  #                         target year
  #   - targets (df)   : tibble of target parameter totals, processed
  # 
  # Returns: matrix with rows representing each tax unit and columns for each 
  #             target. Values are:
  #                   0 (num)  : if the tax unit does not satisfy all of the target conditions
  #       Scaled Weight (num)  : if the tax unit satisfies all the target conditions 
  #----------------------------------------------------------------------------  
  
  
  # Construct output matrix
  lhs = matrix(nrow = nrow(puf), ncol = nrow(targets))
  
  for (i in 1:nrow(targets)) {
    
    # Select target row
    this_constraint = targets[i,]
    
    # Target variable
    variable = this_constraint[["variable"]]
    
    # Valid filing statuses
    filing_status = this_constraint[["filing_status"]] %>%
      str_split_1(' ') %>%
      as.numeric()
    
    # Valid age groups
    age_group = this_constraint[["age_group"]] %>%
      str_split_1(' ') %>%
      as.numeric()
    
    # Minimum and maximum AGIs
    agi_min = this_constraint[["agi_min"]]
    agi_max = this_constraint[["agi_max"]]
    
    # Adjust weight to accurate decimal place, then use binary flags to check validity
    column = puf$weight * 
      puf[[variable]] * 
      (puf$filing_status %in% filing_status) * 
      (puf$age_group %in% age_group) * 
      (puf$E00100 >= agi_min) * 
      (puf$E00100 < agi_max)
    
    # Add target variable as a column to matrix
    lhs[,i] = column
  }
  
  return(lhs)
  
}



run_lp = function(lhs, rhs, e, e_runs) {
  
  #----------------------------------------------------------------------------
  # Runs Linear Programming solver to generate minimized scalars that 
  #  satisfies the targets.
  # 
  # Parameters:
  #   - lhs (matrix) : Sparse matrix of scaled tax unit weights
  #   - rhs (df)     : tibble of target parameter totals, processed
  #   - e (dbl)      : episilon value; if NULL, find optimal value using e_runs
  #   - e_runs (int) : number of iterations to attempt to find ideal maximum
  #                    deviation from observed weights
  # 
  # Returns: vector of weight adjustment factors if solved; NULL otherwise
  #----------------------------------------------------------------------------  
  
  
  # Construct solver reference
  lprw = make.lp(ncol(lhs), nrow(lhs))
  
  # Objective Function
  set.objfn(lprw, rep(1, nrow(lhs)))
  
  # Set up targets
  rhs %<>%
    mutate(upper = target_value * (1 + tolerance),
           lower = target_value * (1 - tolerance))
  
  # Add each column of the left hand side as the constant for decision variable 
  # constraints, less than or equal to upper target range, less than or equal to lower
  for (i in 1:ncol(lhs)) {
    add.constraint(lprw,
                   lhs[,i],
                   "<=",
                   rhs$upper[i]
    )
    
    add.constraint(lprw,
                   lhs[,i],
                   ">=",
                   rhs$lower[i]
    )
  }
  
  # Set epsilon (maximum percent deviation from observed weights). If not 
  # supplied, search for optimal value
  epsilon = e 
  if (is.null(e)) {
    epsilon = tune_epsilon(lprw, nrow(lhs), ncol(lhs), e_runs)
  }
  
  # Set boundaries for new weight deviation from observed
  set.bounds(lprw,
             lower = rep(1 - epsilon, nrow(lhs), columns = c(1:ncol(lhs))),
             upper = rep(1 + epsilon, nrow(lhs), columns = c(1:ncol(lhs)))
  )
  
  # Attempt to solve system of equations
  solution = solve(lprw)
  
  # Return reweighting factors if a solution could be found; NULL if not
  if (solution == 0) {
    return(get.variables(lprw))
  } else {
    return(NULL)
  }
}



tune_epsilon = function(lp, ncol, iters) {
  
  #----------------------------------------------------------------------------
  # Conducts binary search to find the tightest boundaries for scalars. Done to 
  #  ensure that synthetic weights differ as little as possible from reported values.
  # 
  # Parameters:
  #   - lp (reference)       : Linear Programming solver with objective function
  #                               and constraint matrix.
  #   - ncol (num)           : Number of columns in lp (equivalent to number of
  #                               tax units)
  #   - iters (num)          : number of iterations 
  # 
  # Returns:  Epsilon (num)  : Ideal boundary delta
  #----------------------------------------------------------------------------  
  
  
  # Initialize binary search
  low     = 0
  test    = 0.5
  epsilon = 0.5
  high    = 1
  
  for (i in 1:iters) {
    
    # Set decision variable boundaries with the test epsilon
    set.bounds(lp, lower = rep(1 - test, ncol), columns = c(1:ncol)) 
    set.bounds(lp, upper = rep(1 + test, ncol), columns = c(1:ncol))
    
    # Check if solver finds a valid solution and narrow search field dependent on success
    if (solve(lp) == 0){
      high    = test
      epsilon = test
      test    = (test - low) / 2 + low
    } else {
      l    = test
      test = (high - test) / 2 + test
    }
  }
  
  # Display and return the ideal epsilon
  print(paste0("Ideal Epsilon is ", epsilon))
  
  return(epsilon)
}




