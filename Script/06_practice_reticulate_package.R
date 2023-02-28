################################################################################
# TITLE        :: PRACTICE RETICULATE PACKAGE
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2022-09-24 (TUE)
# UPDATED DATE :: 2023-01-16 (MON)

################################################################################
#00-1. SET WORK ENVIRONMNET
#(1) Import packages
library(tidyverse)
library(glue)
library(reticulate)

#(2) Set system options
set.seed(seed=2022)
options(scipen=100)
options(tibble.print_min=100)
options(doFuture.rng.onMisuse="ignore")

#(3) Import python packages 
np <- import(module="numpy")
pd <- import(module="pandas")

################################################################################
#01. PRACTICE NUMPY MODULE
#(1) Define numpy-array
array_01 <- np$array(object=c(1, 2, 3, 5, 6, 7, 3, 4, 9))
array_02 <- np$arange(start=1, stop=10+1)
matrix_01 <-np$reshape(a=array_01, newshape=c(3L, 3L))

#(2) Solve inverse matrix 
matrix_01_inv <- np$linalg$inv(a=matrix_01)

#(3) Find solution set from linear system
np$linalg$solve(a=matrix_01, b=matrix(data=c(3, 5, 7), nrow=3))

################################################################################
#02. PRACTICE PANDAS PACKAGE
#(1) Read csv files
USEDCAR_train_raw <- pd$read_csv(
  filepath_or_buffer="/Users/CSH/Workspace/R/Projects/Dacon_usedcar_price/Data/raw/train.csv"
)
USEDCAR_test_raw <- pd$read_csv(
  filepath_or_buffer="/Users/CSH/Workspace/R/Projects/Dacon_usedcar_price/Data/raw/test.csv"
)

#(2) Concatenate datasets
USEDCAR_raw <- pd$concat(objs=list(USEDCAR_train_raw, USEDCAR_test_raw), axis=0)

#(3) Transform pandas dataframe into tidyverse tibble
USEDCAR_tbl <- USEDCAR_raw %>% 
  as_tibble()

################################################################################