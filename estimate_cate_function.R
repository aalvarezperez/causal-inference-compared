
fit_learner. <- function(y, w, X, learner, num_search_rounds, n_folds, W.hat, Y.hat){
  switch(
    learner,
    S = sboost(
      X,w,y,
      k_folds = n_folds,
      num_search_rounds = num_search_rounds,
      verbose = FALSE
    ),
    T = tboost(
      X,w,y,
      k_folds_mu1 = n_folds,
      k_folds_mu0 = n_folds,
      num_search_rounds = num_search_rounds,
      verbose = FALSE
    ),
    X = xboost(
      X,w,y,
      k_folds_mu1 = n_folds,
      k_folds_mu0 = n_folds,
      k_folds_p = n_folds, 
      num_search_rounds = num_search_rounds,
      verbose = FALSE
    ),
    R = rboost(
      X,w,y,
      k_folds = n_folds,
      num_search_rounds = num_search_rounds,
      verbose = FALSE
    ),
    causal_forest = grf::causal_forest(
      Y = y,
      X = X,
      W = w, 
      tune.parameters = "all",
      W.hat = W.hat,
      Y.hat = Y.hat
      )
  )
}


sample_data. <- function(data, args){
  
  set.seed(42)
  
  if(is.null(args$sample_prop)){
    if(nrow(data) > 50e3){
      msg_text <- sprintf("More than 50k rows available: defaulting to training on 50k rows. Use sample_prop otherwise.")
      message(msg_text)
      sample_index <- sample(1:nrow(data), size = 50e3, replace = FALSE)
      data_sampled <- data[sample_index, ]
    } else{
      msg_text <- sprintf("Less than 50k rows available: defaulting to training on full dataset (n = %i). Use sample_prop otherwise.", nrow(data))
      message(msg_text)
      data_sampled <- data
    }
  } else{
    if(!(is.numeric(args$sample_prop) & (args$sample_prop > 0 & args$sample_prop <= 1))) {
      stop("Sample_prop must be numeric, higher than 0 and lower or equal to 1")
    }
    
    sample_message <- sprintf("sample_prop is provided: training on %s of the observations.", scales::percent(sample_prop))
    message(sample_message)
    
    sample_index <- sample(1:nrow(data), size = nrow(data)*args$sample_prop, replace = FALSE)
    data_sampled <- data[sample_index, ]
  }
  data_sampled
}

sanitise_data. <- function(data, args){

  include_vars_index <- which(names(data) %in% c(args$y, args$w, args$X))
  data <- data[, include_vars_index]
  
  original_covariate_names <- names(data)

  data[, which(names(data) == args$w)] <- 1*(data[, which(names(data) == args$w)] == args$test_group)
  
  list(
    clean_data = data,
    original_covariate_names = original_covariate_names
  )
}




estimate_heterogenous_effects <- function(data,
                                          y, w, X,
                                          test_group, 
                                          sample_prop = NULL,
                                          num_search_rounds = 10,
                                          n_folds = 3,
                                          Y.hat = NULL,
                                          W.hat = NULL,
                                          learner = c("S", "T", "X", "R", "causal_forest")
                                          ){
  
  require(rlearner)
  require(dplyr)
  
  env <- list(
    w = w, y = y, X = X, test_group = test_group, learner = learner, sample_prop = sample_prop
  )
  
  data_sampled <- sample_data.(data, args = env)
  
  clean_data_list <- sanitise_data.(data_sampled, args = env)
  
  clean_data_df <- clean_data_list$clean_data
  
  y <- clean_data_df[[y]]
  w <- clean_data_df[[w]]
  X <- as.matrix(clean_data_df[X])

  fit_learner.(y, w, X, learner)
}
