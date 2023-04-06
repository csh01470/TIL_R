################################################################################
# TITLE        :: PRACTICE ONE-CLASS SVC MODEL 
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2023-03-29 (WED)
# UPDATED DATE :: 2023-03-29 (WED)

################################################################################
#00. SET WORK ENVIRONMENT
#(1) Import package
library(tidyverse)
library(tidymodels)
library(dlookr)
library(janitor)
library(kernlab)
library(future)
library(doFuture)

#(2) Set system option
set.seed(seed=2023)
options(scipen=100)

################################################################################
#00-2. ASSIGN USER-DEFINE FUNCTIONS
#(1) Define `predict_ksvm()` function 
predict_ksvm <- function(object, new_data, type){
  return_value <- predict(object=object, newdata=new_data, type=type) %>% 
    as_tibble_col() %>% 
    rename(class=value) %>% 
    mutate(
      class=ifelse(test=(class==TRUE), yes=(0), no=(1)),
      class=as_factor(x=class)
    )
  return(return_value)
}

################################################################################
#01. READ AND EDA DATASET
#(1) Read `credit_raw` datasets
credit_raw <- read_csv(file="https://raw.githubusercontent.com/Datamanim/datarepo/main/adp/27/problem1.csv",
                       guess_max=Inf) %>% 
  clean_names() %>% 
  mutate(class=as.factor(x=class))

#(2) Fit PCA for dimensionally reduction
credit_pca <- recipe(formula=~., data=credit_raw) %>% 
  step_normalize(all_numeric()) %>% 
  step_pca(all_numeric(), threshold=0.8) %>% 
  prep() %>% 
  bake(new_data=NULL) 

#(3) Separate train-validation-test split
credit_split <- initial_split(data=credit_pca, prop=0.75, strata=class)
credit_train <- credit_split %>% 
  training() %>% 
  initial_split(data=., prop=0.75) %>%
  training() %>% 
  filter(class=="0")
credit_validation <- credit_split %>% 
  training() %>% 
  initial_split(data=., prop=0.75) %>%
  testing() 
credit_test <- credit_split %>% 
  testing()

################################################################################
#02. TRAIN AND TEST BY RAW MODEL
#(1) Fit train dataset 
svm_OC_raw <- ksvm(
  x=class~., 
  data=credit_train,
  type="one-svc",
  kernel="rbfdot",
  kpar=list(sigma=0.2),
  prop.model=TRUE,
  nu=0.05
)

#(2) Predict validation dataset
svm_OC_raw_val_predict <- predict_ksvm(object=svm_OC_raw, new_data=credit_validation, type="response")

#(3) Check accuracy
accuracy_vec(truth=credit_validation[["class"]], estimate=svm_OC_raw_val_predict[["class"]])

#(4) Predict test dataset
svm_OC_raw_test_predict <- predict_ksvm(object=svm_OC_raw, new_data=credit_test, type="response")

#(5) Check accuracy
accuracy_vec(truth=credit_test[["class"]], estimate=svm_OC_raw_test_predict[["class"]])

################################################################################
#03. TRAIN AND TEST BY TIDYMODEL-PARSNIP OBJECT
#(1) Define model
svm_OC_model <- svm_rbf(
  rbf_sigma = tune()
) %>%
  set_mode(mode="classification") %>%
  set_engine(engine="kernlab",
             type="one-svc")

#(2) Define workflow
svm_OC_wf <- workflow(
  preprocessor = class~.,
  spec         = svm_OC_model
)

#(3) Define hyper-parameter
svm_OC_params <- svm_OC_model %>%
  extract_parameter_set_dials()

#(4) Tune hyper-parameters
svm_OC_tune <- tune_grid(
  object    = svm_OC_wf,
  grid      = grid_latin_hypercube(x=svm_OC_params, size=100),
  resamples = validation_split(data=credit_train, prop=0.75, strata=class),
  metrics   = metric_set(roc_auc),
  control   = control_grid(verbose=TRUE,
                           save_pred=TRUE,
                           save_workflow=TRUE,
                           parallel_over="everything")
)

#MEMO. WHY DOES NOT WORK ??

################################################################################