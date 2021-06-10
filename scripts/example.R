require(pacman)
source("estimate_cate_function.R")
p_load(
  dplyr,
  DALEX,
  grf
  )

################
#### Data ######
################

# we work with lalonde today
lalonde <- MatchIt::lalonde

# create dummies for 'race'. 
lalonde <- fastDummies::dummy_cols(lalonde) %>%
  select(-race)

# check that
names(lalonde)

covs <- c("age", "educ", "married", "nodegree", "re74", "re75", "race_black", "race_hispan", "race_white")



################# 
##### Model #####
#################

# this wraps around several methods to estimate CATE. We use causal_forest today.
tau_lalonde <- estimate_heterogenous_effects(
  data = lalonde,
  y = "re78",
  w = "treat",
  test_group = 1,
  X = covs,
  learner = "causal_forest"
)

# here are our estimated effects for each subject
predict(tau_lalonde)
preds <- tau_lalonde$predictions 
lalonde$effect <- preds
preds %>% hist(breaks = 50)



##################################
#### Inference and explanation ###
##################################

# we can answer right away ...
grf::average_treatment_effect(tau_lalonde, target.sample = "all") # ... what is the ATE, as the average CATE 
mean(preds > 0) #... what share of the subjects are estimated to have a 'positive' impact of the treatment (100% here)

# or we can summarise the CATE functions as tau(x) = E[Y(1) - Y(0) | X] with a linear model:
# tau(X) ~ beta0 + A * beta. This tells us how CATE relates to some variables.
best_linear_projection(tau_lalonde, lalonde[covs]) # here we use the same ones we used for the causal forest.


# we can explain CATE in other ways too: https://dalex.drwhy.ai/ ...

# create an explainer for the function of CATE
explainer_cate <- explain(
  model = tau_lalonde, # model that learned the function of CATE
  data = lalonde, # ideally this is unseen data
  y = lalonde$effect, # the estimated effect. note: this can never be the ground truth. that does not exist. We can only estimate it.
  predict_function = function(model, newdata) predict(model, newdata[covs])$predictions, 
  label = "causal forest"
    )


# explain on dataset level:
plot(predict_profile(explainer_cate, new_observation = lalonde[2,covs], variables = covs, type = "ceteris_paribus")) # age seems to be important
plot(variable_profile(explainer_cate, variables = "age")) # subjects above 20 years see a positive impact on their susceptibility to treatment

# explain on subject-level
harry <- harry_made_younger <- lalonde[3,covs] # the third obersation in the dataset
harry_made_younger$age <- 20 # surgical intervention on age

# harry prediction
plot(predict_parts(explainer_cate, new_observation = harry, type = "break_down_interactions")) 

# harry's prediction, had he been 20 years, instead of 30
plot(predict_parts(explainer_cate, new_observation = harry_made_younger, type = "break_down_interactions"))


# These methods offer mainly 2 advantages for AB testing in eCG:
# - insight in impact heterogeneity of product changes
#   - statements like '40% of the users saw a negative impact' can be made, or;
#   - '40% of the users saw a negative impact, even if on average we saw a positive effect'
#   - '80% of the estimated gain in metric x due to treatment T, sits in 20% of the users'
# - explain heterogeneity by user characterisitcs
#   - statements like 'we see that users who had less than 10 VIPs in interval p before treatment, show lower benefit from the treatment than their complement'
#   - opens the door for personalised recommendations of treatment: rolling out different versions of a product for different users
