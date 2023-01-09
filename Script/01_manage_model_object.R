################################################################################
# TITLE        :: MANAGE ML_MODEL OBJECT
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2022-09-14 (WED)
# UPDATED DATE :: 2023-01-07 (WED)

################################################################################
#00-1. SET WORK ENVIRONMNET
#(1) Import packages
library(tidyverse)
library(tidymodels)
library(future)
library(doFuture)
library(glue)

#(2) Set system options
set.seed(seed=2023)
options(scipen=100)
options(tibble.print_min=100)
options(doFuture.rng.onMisuse="ignore")

#(3) Set parallel process
THREAD_NUM <- unname(obj=availableCores())-2
plan(strategy=multisession, workers=THREAD_NUM)
registerDoFuture()

################################################################################
#01. READ AND PREPARE DATASET FOR MACHINE LEARNING
#(1) Read diamonds dataset
diamonds <- ggplot2::diamonds %>% 
  sample_n(size=50000)

#(2) Split dataset
diamonds_split <- initial_split(data=diamonds, prop=0.75)
diamonds_train <- diamonds_split %>% 
  training()
diamonds_test <- diamonds_split %>% 
  testing()

#(3) Define 5-fold CV
diamonds_train_cv <- vfold_cv(data=diamonds_train, v=5)

#(4) Define recipes
diamonds_recipe <- recipe(formula=price~., data=diamonds_train) %>% 
  step_dummy(all_nominal_predictors())

################################################################################
#02. TRAIN AND PREDICT WITH GLMNET MODEL
#(1) Define model
glmnet_model <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_mode(mode="regression") %>%
  set_engine(engine="glmnet")

#(2) Define workflow
glmnet_wf <- workflow(
  preprocessor = diamonds_recipe,
  spec         = glmnet_model
)

#(3) Define hyper-parameter
glmnet_params <- parameters(
  penalty(),
  mixture()
)

#(4) Tune hyper-parameters by grid search
glmnet_tune <- tune_grid(
  object    = glmnet_wf,
  grid      = grid_random(x=glmnet_params, size=100),
  resamples = diamonds_train_cv,
  metrics   = metric_set(rmse),
  control   = control_grid(verbose=TRUE,
                           save_pred=TRUE,
                           save_workflow=TRUE,
                           parallel_over="everything")
)

#(5) Print best score and optimal hyper-parameters
show_best(x=glmnet_tune, metric="rmse", n=10)

#(6) Show autoplot
autoplot(object=glmnet_tune, metric="rmse") + 
  theme_bw()

#(7) Optimize workflow and hyper-parameters
glmnet_wf_best <- finalize_workflow(x=glmnet_wf, parameters=select_best(x=glmnet_tune))

#(8) Fit train dataset
glmnet_fit <- fit(object=glmnet_wf_best, data=diamonds_train)

#(9) Predict test dataset
glmnet_predict <- predict(object=glmnet_fit, new_data=diamonds_test)

#(10) Check RMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=glmnet_predict[[".pred"]])

#(11) Check NRMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=glmnet_predict[[".pred"]]) / 
  mean(x=glmnet_predict[[".pred"]]) * 100

#PLUS) Stop cluster
plan(strategy=sequential)

################################################################################
#03. SAVE MODEL  
#(1) Define variables
TODAY <- Sys.Date() %>% 
  str_replace_all(string=., pattern="[^0-9]", replacement="")
MODEL_NM <- ls() %>% 
  str_extract(string=., pattern="[A-Za-z_]+_model") %>% 
  str_replace(string=., pattern="_model", replacement="") %>% 
  subset(x=., !is.na(x=.))
DIR_PATH <- "Output/"

#(2) Make directory
if(!dir.exists(path=DIR_PATH)){
  dir.create(path=DIR_PATH)
}

#(3) Save tuned model
write_rds(x=eval(expr=parse(text=glue("{MODEL_NM}_tune"))), 
          file=glue('{DIR_PATH}{MODEL_NM}_tune_{TODAY}.rds'))

#(4) Save fitted model
write_rds(x=eval(expr=parse(text=glue("{MODEL_NM}_fit"))), 
          file=glue('{DIR_PATH}{MODEL_NM}_fit_{TODAY}.rds'))

################################################################################
#04-1. READ TUNED MODEL AND CHECK PERFORMANCE
#(1) Remove model object 
rm(glmnet_tune, glmnet_wf_best, glmnet_fit, glmnet_predict)

#(2) Read tuned-model object
glmnet_tune <- read_rds(
  file=glue('{DIR_PATH}/{MODEL_NM}_tune_{TODAY}.rds')
)

#(3) Optimize model
glmnet_wf_best <- finalize_workflow(x=glmnet_wf, parameters=select_best(x=glmnet_tune))

#(4) Fit train dataset
glmnet_fit <- fit(object=glmnet_wf_best, data=diamonds_train)

#(5) Predict test dataset
glmnet_predict <- predict(object=glmnet_fit, new_data=diamonds_test) 

#(6) Check RMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=glmnet_predict[[".pred"]])

#(7) Check NRMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=glmnet_predict[[".pred"]]) / 
  mean(x=glmnet_predict[[".pred"]]) * 100

################################################################################
#04-2. READ FITTED MODEL AND CHECK PERFORMANCE
#(1) Remove model object 
rm(glmnet_fit, glmnet_predict)

#(2) Read fitted-model object
glmnet_fit <- read_rds(
  file=glue('{DIR_PATH}/{MODEL_NM}_fit_{TODAY}.rds')
)

#(3) Predict test dataset
glmnet_predict <- predict(object=glmnet_fit, new_data=diamonds_test) 

#(4) Check RMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=glmnet_predict[[".pred"]])

#(5) Check NRMSE
rmse_vec(truth=diamonds_test[["price"]], estimate=glmnet_predict[[".pred"]]) / 
  mean(x=glmnet_predict[[".pred"]]) * 100

################################################################################