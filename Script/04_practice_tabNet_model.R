################################################################################
# TITLE        :: PRACTICE TABNET MODEL
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2022-09-17 (SAT)
# UPDATED DATE :: 2023-01-10 (TUE)

################################################################################
#00. SET WORK ENVIRONMNET
#(1) Import packages
library(tidyverse)
library(tidymodels)
library(tabnet)
library(glue)

#(2) Set system options
set.seed(seed=2023)
options(scipen=100)
options(tibble.print_min=100)

################################################################################
#01. READ AND PREPARE DATASET FOR MACHINE LEARNING
#(1) Read diamonds dataset
diamonds <- ggplot2::diamonds

#(2) Split and make dataset
diamonds_split <- initial_split(data=diamonds, prop=0.75)
diamonds_train <- diamonds_split %>% 
  training()
diamonds_test <- diamonds_split %>% 
  testing()

#(3) Define 5-fold CV
diamonds_train_cv <- vfold_cv(data=diamonds_train, v=5)

#(4) Define Recipe
diamonds_recipe <- recipe(formula=price~., data=diamonds_train) %>% 
  step_dummy(all_nominal_predictors())

################################################################################
#02. TRAIN AND PREDICT WITH TABNET MODEL
#(1) Define model
tabnet_model <- tabnet(
  epochs          = tune(),
  decision_width  = tune(),
  attention_width = tune(),
  num_step        = tune(),
  num_independent = tune(),
  num_shared      = tune()
) %>% 
  set_mode(mode="regression") %>%
  set_engine(engine="torch")

#(2) Define workflow
tabnet_wf <- workflow(
  preprocessor = diamonds_recipe,
  spec         = tabnet_model
)

#(3) Define hyper-parameters
tabnet_params <- parameters(
  epochs(),
  decision_width(),
  attention_width(),
  num_steps(),
  num_independent(),
  num_shared()
)

#(4) Tune hyper-parameters by bayesian optimization
tabnet_tune <- tune_bayes(
  object     = tabnet_wf,
  param_info = tabnet_params,
  resamples  = diamonds_train_cv,
  iter       = 100,
  initial    = 10,
  metrics    = metric_set(rmse),
  control    = control_bayes(verbose=TRUE,
                             save_pred=TRUE,
                             save_workflow=TRUE,
                             no_improve=25,
                             allow_par=FALSE)
)

#(5) Print best score and optimal hyper-parameters
show_best(x=tabnet_tune, metric="rmse", n=10)

#(6) Show autoplot
autoplot(object=tabnet_tune, metric="rmse") + 
  theme_bw()

#(7) Optimize hyper-parameters
tabnet_wf_best <- finalize_workflow(x=tabnet_wf, parameters=select_best(x=tabnet_tune))

#(8) Fit train dataset
tabnet_fit <- fit(object=tabnet_wf_best, data=diamonds_train)

#(9) Predict test dataset
tabnet_predict <- predict(object=tabnet_fit, new_data=diamonds_test) 

#(10) Check RMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=tabnet_predict[[".pred"]])

#(11) Check NRMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=tabnet_predict[[".pred"]]) / 
  mean(x=tabnet_predict[[".pred"]]) * 100

################################################################################