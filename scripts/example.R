require(pacman)
p_load(dplyr)
source("estimate_cate_function.R")

lalonde <- MatchIt::lalonde
lalonde <- fastDummies::dummy_cols(lalonde) %>%
  select(-race)

names(lalonde)

covs <- c("age", "educ", "married", "nodegree", "re74", "re75", "race_black", "race_hispan", "race_white")

tau_lalonde <- estimate_heterogenous_effects(
  data = lalonde,
  y = "re78",
  w = "treat",
  test_group = 1,
  X = covs,
  learner = "R.cf"
)

preds <- tau_lalonde$predictions 
preds %>% hist(breaks = 50)
mean(preds)
mean(preds > 0)

grf::best_linear_projection(forest = tau_lalonde, A = lalonde[covs])

