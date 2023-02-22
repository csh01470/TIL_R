################################################################################
# TITLE        :: PRACTICE MARS MODEL
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2022-12-02 (FRI)
# UPDATED DATE :: 2022-01-08 (SUN)

################################################################################
#00. SET WORK ENVIRONMENT
#(1) Import packages
library(tidyverse)
library(tidymodels)
library(baguette)
library(future)
library(doFuture)
library(glue)

#(2) Set system options
set.seed(seed=2023)
options(scipen=100)
options(doFuture.rng.onMisuse="ignore")

#(3) Set parallel process
THREAD_NUM <- unname(obj=availableCores())-2
plan(strategy=multisession, workers=THREAD_NUM)
registerDoFuture()

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
#02. TRAIN AND PREDICT WITH MARS MODEL
#(1) Define model
mars_model <- mars(
  num_terms   = tune(),
  prod_degree = tune(),
  prune_method = tune()
) %>% 
  set_mode(mode="regression") %>% 
  set_engine(engine="earth",
             nfold=5)

#(2) Define workflow
mars_wf <- workflow(
  preprocessor = diamonds_recipe,
  spec         = mars_model
)

#(3) Define hyper-parameters
mars_params <- parameters(
  num_terms(),
  prod_degree(),
  prune_method()
) %>% 
  finalize(diamonds_train)

#(4) Tune hyper-parameters by grid search
mars_tune <- tune_grid(
  object     = mars_wf,
  grid       = grid_latin_hypercube(x=mars_params, size=100),
  resamples  = diamonds_train_cv,
  metrics    = metric_set(rmse),
  control    = control_grid(verbose=FALSE,
                            save_pred=TRUE,
                            save_workflow=TRUE,
                            parallel_over="everything")
)

#(5) Print best score and optimal hyper-parameters
show_best(x=mars_tune, metric="rmse", n=10)

#(6) Show autoplot
autoplot(object=mars_tune, metric="rmse") + 
  theme_bw()

#(7) Optimize workflow and hyper-parameters
mars_wf_best <- finalize_workflow(x=mars_wf, parameters=select_best(x=mars_tune)) 

#(8) Fit train dataset
mars_fit <- fit(object=mars_wf_best, data=diamonds_test)

#(9) Predict test dataset
mars_predict <- predict(object=mars_fit, new_data=diamonds_test)

#(10) Check RMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=mars_predict[[".pred"]])

#(11) Check NRMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=mars_predict[[".pred"]]) / 
  mean(x=mars_predict[[".pred"]]) * 100

################################################################################
#03. TRAIN AND PREDICT WITH MARS BAGGING MODEL
#(1) Define model
bagging_mars_model <- bag_mars(
  num_terms   = tune(),
  prod_degree = tune(),
  prune_method = tune()
) %>% 
  set_mode(mode="regression") %>% 
  set_engine(engine="earth",
             nfold=5,
             times=10)

#(2) Define workflow
bagging_mars_wf <- workflow(
  preprocessor = diamonds_recipe,
  spec         = bagging_mars_model
)

#(3) Define hyper-parameters
bagging_mars_params <- parameters(
  num_terms(),
  prod_degree(),
  prune_method()
) %>% 
  finalize(diamonds_train)

#(4) Tune hyper-parameters by grid search
bagging_mars_tune <- tune_grid(
  object     = bagging_mars_wf,
  grid       = grid_latin_hypercube(x=bagging_mars_params, size=100),
  resamples  = diamonds_train_cv,
  metrics    = metric_set(rmse),
  control    = control_grid(verbose=FALSE,
                            save_pred=TRUE,
                            save_workflow=TRUE,
                            parallel_over="everything")
)

#(5) Print best score and optimal hyper-parameters
show_best(x=bagging_mars_tune, metric="rmse", n=10)

#(6) Show autoplot
autoplot(object=bagging_mars_tune, metric="rmse") + 
  theme_bw()

#(7) Optimize workflow and hyper-parameters
bagging_mars_wf_best <- finalize_workflow(x=bagging_mars_wf, parameters=select_best(x=bagging_mars_tune)) 

#(8) Fit train dataset
bagging_mars_fit <- fit(object=bagging_mars_wf_best, data=diamonds_test)

#(9) Predict test dataset
bagging_mars_predict <- predict(object=bagging_mars_fit, new_data=diamonds_test)

#(10) Check RMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=bagging_mars_predict[[".pred"]])

#(11) Check NRMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=bagging_mars_predict[[".pred"]]) / 
  mean(x=bagging_mars_predict[[".pred"]]) * 100

#PLUS) Stop parallel process
plan(strategy=sequential)

################################################################################