################################################################################
# TITLE        :: PRACTICE HYPER PARAMETER TUNING METHOD
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2022-11-27 (SUN)
# UPDATED DATE :: 2023-02-15 (WED)

################################################################################
#00. SET WORK ENVIRONMENT
#(1) Import packages
library(tidyverse)
library(tidymodels)
library(catsnip)
library(future)
library(doFuture) 
library(glue)

#(2) Set system options
set.seed(seed=2022)
options(scipen=100)
options(doFuture.rng.onMisuse="ignore")

#(3) Set parallel process
THREAD_NUM <- unname(obj=availableCores())-2
plan(strategy=multisession, workers=THREAD_NUM)
registerDoFuture()

################################################################################
#01. READ DATASET AND PREPARE FOR ML
#(1) Read dataset
diamonds <- ggplot2::diamonds 

#(2) Split and make dataset
diamonds_split <- initial_split(data=diamonds, prop=0.75)
diamonds_train <- diamonds_split %>% training()
diamonds_test <- diamonds_split %>% testing()

#(3) Define 5-fold CV
diamonds_train_cv <- vfold_cv(data=diamonds_train, v=5)

#(4) Define Recipe
diamonds_recipe <- recipe(formula=price~., data=diamonds_train) 

################################################################################
#02. DEFINE MODEL OBJECT
#(1) Define model
catboost_model <- boost_tree(
  mtry        = tune(),
  trees       = tune(),
  min_n       = tune(),
  tree_depth  = tune(),
  learn_rate  = tune(),
  sample_size = tune(),
  stop_iter   = tune()
) %>% 
  set_mode(mode="regression") %>% 
  set_engine(engine="catboost")

#(2) Define workflow
catboost_wf <- workflow(
  preprocessor = diamonds_recipe,
  spec         = catboost_model
)

#(3) Define hyper-parameters
catboost_params <- catboost_model %>%
  extract_parameter_set_dials() %>% 
  finalize(x=diamonds_train)

################################################################################
#03-1. USE TUNE_GRID() FUNCTION
#(1) Tune hyper-parameters
catboost_tune_grid_01 <- tune_grid(
  object     = catboost_wf,
  param_info = catboost_params,
  grid       = 900,
  resamples  = diamonds_train_cv,
  metrics    = metric_set(rmse),
  control    = control_grid(verbose=FALSE,
                            save_pred=TRUE,
                            save_workflow=TRUE,
                            parallel_over="resamples")
)

#(2) Show best score 
show_best(x=catboost_tune_grid_01, metric="rmse", n=10)

#(3) Show optimal hyper-parameters
autoplot(object=catboost_tune_grid_01, metric="rmse") + 
  theme_bw()

#(4) Optimize model
catboost_wf_best <- finalize_workflow(x=catboost_wf, 
                                      parameters=select_best(x=catboost_tune_grid_01)) 

#(5) Fit train dataset
catboost_fit <- fit(object=catboost_wf_best, data=diamonds_test)

#(6) Predict test dataset
catboost_predict <- predict(object=catboost_fit, new_data=diamonds_test)

#(7) Check RMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=catboost_predict[[".pred"]])

################################################################################
#03-2. USE TUNE_GRID() AND EXPAND_GRID() FUNTION
#(4) Set grid-search type
catboost_grid_02 <- expand_grid(
  mtry        = as.integer(x=c(3, 4, 5)), 
  trees       = as.integer(x=c(100, 500, 1000)),
  min_n       = 20L,
  tree_depth  = c(6:10),
  learn_rate  = seq(from=0.01, to=0.05, by=0.01),
  sample_size = seq(from=0.7, to=1, by=0.1),
  stop_iter   = 50L
) 

#(2) Tune hyper-parameters
catboost_tune_grid_02 <- tune_grid(
  object     = catboost_wf,
  param_info = catboost_params,
  grid       = catboost_grid_02,
  resamples  = diamonds_train_cv,
  metrics    = metric_set(rmse),
  control    = control_grid(verbose=FALSE,
                            save_pred=TRUE,
                            save_workflow=TRUE,
                            parallel_over="resamples")
)

#(3) Show best score 
show_best(x=catboost_tune_grid_02, metric="rmse", n=10)

#(4) Show optimal hyper-parameters
autoplot(object=catboost_tune_grid_02, metric="rmse") + 
  theme_bw()

#(5) Optimize model
catboost_wf_best <- finalize_workflow(x=catboost_wf, 
                                      parameters=select_best(x=catboost_tune_grid_02)) 

#(6) Fit train dataset
catboost_fit <- fit(object=catboost_wf_best, data=diamonds_test)

#(7) Predict test dataset
catboost_predict <- predict(object=catboost_fit, new_data=diamonds_test)

#(8) Check RMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=catboost_predict[[".pred"]])

################################################################################
#03-3. USE TUNE_GRID() AND GRID_LATIN_HYPERCUBE() FUNTION
#(1) Set grid-search type
catboost_grid_03 <- catboost_params %>%
  grid_latin_hypercube(size=900)

#(2) Tune hyper-parameters
catboost_tune_grid_03 <- tune_grid(
  object     = catboost_wf,
  param_info = catboost_params,
  grid       = catboost_grid_03,
  resamples  = diamonds_train_cv,
  metrics    = metric_set(rmse),
  control    = control_grid(verbose=FALSE,
                            save_pred=TRUE,
                            save_workflow=TRUE,
                            parallel_over="resamples")
)

#(3) Show best score 
show_best(x=catboost_tune_grid_03, metric="rmse", n=10)

#(4) Show optimal hyper-parameters
autoplot(object=catboost_tune_grid_03, metric="rmse", digits=3, scientific=FALSE) + 
  theme_bw()

#(5) Optimize model
catboost_wf_best <- finalize_workflow(x=catboost_wf, 
                                      parameters=select_best(x=catboost_tune_grid_03)) 

#(6) Fit train dataset
catboost_fit <- fit(object=catboost_wf_best, data=diamonds_test)

#(7) Predict test dataset
catboost_predict <- predict(object=catboost_fit, new_data=diamonds_test)

#(8) Check RMSE Value
rmse_vec(truth=diamonds_test[["price"]], estimate=catboost_predict[[".pred"]])

################################################################################
#04. USE TUNE_BAYES() FUNCTION 
#(1) Tune hyper-parameters
catboost_tune_bayes <- tune_bayes(
  object     = catboost_wf,
  param_info = catboost_params,
  resamples  = diamonds_train_cv,
  iter       = 100,
  metrics    = metric_set(rmse),
  initial    = 10,
  control    = control_bayes(verbose=TRUE,
                             save_pred=TRUE,
                             save_workflow=TRUE,
                             no_improve=25,
                             parallel_over="resamples")
)

#(2) Show best score 
show_best(x=catboost_tune_bayes, metric="rmse", n=10)

#(4) Show optimal hyper-parameters
autoplot(object=catboost_tune_bayes, metric="rmse", digits=3, scientific=FALSE) + 
  theme_bw()

#(5) Optimize model
catboost_wf_best <- finalize_workflow(x=catboost_wf, 
                                      parameters=select_best(x=catboost_tune_bayes)) 

#(6) Fit train dataset
catboost_fit <- fit(object=catboost_wf_best, data=diamonds_test)

#(7) Predict test dataset
catboost_predict <- predict(object=catboost_fit, new_data=diamonds_test)

#(8) Check RMSE Value
rmse_vec(truth=diamonds_test[["price"]], estimate=catboost_predict[[".pred"]])

#PLUS) Stop Cluster
plan(strategy=sequential)

################################################################################