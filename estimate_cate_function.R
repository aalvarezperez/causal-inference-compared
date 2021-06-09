
fit_learner. <- function(y, w, X, learner){
  n_folds <- 5
  num_search_rounds <- 10
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
    R.cf = grf::causal_forest(
      Y = y,
      X = X,
      W = w, 
      tune.parameters = "all", 
      )
  )
}


sample_data. <- function(data, sample_prop){
  
  set.seed(42)
  
  if(is.null(sample_prop)){
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
    if(!(is.numeric(sample_prop) & (sample_prop > 0 & sample_prop <= 1))) {
      stop("Sample_prop must be numeric, higher than 0 and lower or equal to 1")
    }
    
    sample_message <- sprintf("sample_prop is provided: training on %s of the observations.", scales::percent(sample_prop))
    message(sample_message)
    
    sample_index <- sample(1:nrow(data), size = nrow(data)*sample_prop, replace = FALSE)
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




estimate_heterogenous_effects <- function(data, y, w, X, test_group, sample_prop = NULL, learner = c("S", "T", "X", "R", "R.cf")){
  
  require(rlearner)
  require(dplyr)
  
  env <- list(
    w = w, y = y, X = X, test_group = test_group, learner = learner, sample_prop = sample_prop
  )
  
  data_sampled <- sample_data.(data, env$sample_prop)
  
  clean_data_list <- sanitise_data.(data_sampled, args = env)
  
  clean_data_df <- clean_data_list$clean_data
  
  y <- clean_data_df[[y]]
  w <- clean_data_df[[w]]
  X <- as.matrix(clean_data_df[X])

  fit_learner.(y, w, X, learner)
}

covs <- c("fixed_acidity", "volatile_acidity", "citric_acid", 
  "residual_sugar", "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", 
  "density", "pH", "sulphates", "alcohol")

x_test <- estimate_heterogenous_effects(
  data = dat_clean,
  y = "quality",
  w = "color",
  test_group = "red",
  X = covs,
  learner = "X"
  # sample_prop = .1
  )

r_test <- estimate_heterogenous_effects(
  data = dat_clean,
  y = "quality",
  w = "color",
  test_group = "red",
  X = covs,
  learner = "R.cf"
)

r_test2 <- estimate_heterogenous_effects(
  data = dat_clean,
  y = "quality",
  w = "color",
  test_group = "red",
  X = covs,
  learner = "R"
)




df <- MatchIt::lalonde
df <- fastDummies::dummy_cols(df) %>%
  select(-race)
names(df)

covs2 <- c("age", "educ", "married", "nodegree", "re74", "re75", "race_black", "race_hispan", "race_white")
tau_lalonde <- estimate_heterogenous_effects(
  data = df,
  y = "re78",
  w = "treat",
  test_group = 1,
  X = covs2,
  learner = "R.cf"
)


tau_lalonde$predictions %>% 
  hist(breaks = 50)

mean(predict(tau_lalonde, as.matrix(df[covs2]))$predictions)



# Train a standard survival forest.
n <- 2000
p <- 5
X <- matrix(rnorm(n * p), n, p)
failure.time <- exp(0.5 * X[, 1]) * rexp(n)
censor.time <- 2 * rexp(n)
Y <- pmin(failure.time, censor.time)
D <- as.integer(failure.time <= censor.time)
s.forest <- grf::survival_forest(X, Y, D)

# Predict using the forest.
X.test <- matrix(0, 3, p)
X.test[, 1] <- seq(-2, 2, length.out = 3)
s.pred <- predict(s.forest, X.test)

# Plot the survival curve.
plot(NA, NA, xlab = "failure time", ylab = "survival function",
     xlim = range(s.pred$failure.times),
     ylim = c(0, 1))
for(i in 1:3) {
  lines(s.pred$failure.times, s.pred$predictions[i,], col = i)
  s.true = exp(-s.pred$failure.times / exp(0.5 * X.test[i, 1]))
  lines(s.pred$failure.times, s.true, col = i, lty = 2)
}

# Predict on out-of-bag training samples.
s.pred <- predict(s.forest)

# Plot the survival curve for the first five individuals.
matplot(s.pred$failure.times, t(s.pred$predictions[1:5, ]),
        xlab = "failure time", ylab = "survival function (OOB)",
        type = "l", lty = 1)

# Train the forest on a less granular grid.
failure.summary <- summary(Y[D == 1])
events <- seq(failure.summary["Min."], failure.summary["Max."], by = 0.1)
s.forest.grid <- grf::survival_forest(X, Y, D, failure.times = events)
s.pred.grid <- predict(s.forest.grid)
matpoints(s.pred.grid$failure.times, t(s.pred.grid$predictions[1:5, ]),
          type = "l", lty = 2)

