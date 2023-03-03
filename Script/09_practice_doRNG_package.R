################################################################################
# TITLE        :: PRACTICE DORNG AND PURRR PACKAGE 
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2023-01-29 (SUN)
# UPDATED DATE :: 2023-02-08 (WED)

################################################################################
#00. SET WORK ENVIRONMNET
#(1) Import packages
library(tidyverse)
library(glue)
library(readxl)
library(writexl)
library(httr)
library(future)
library(doFuture)
library(doRNG)

#(2) Set system options
options(scipen=100)
options(doFuture.rng.onMisuse="ignore")

#(3) Define pre-variables
read_file_path <- "Data/API_REQUEST/20230208.xlsx"
api_endpoint <- "Key/API_ENDPOINT.txt"

################################################################################
#00-2. ASSIGN USER-DEFINE FUNCTION
#(1) Define SET_PARALLEL_PRC() function
SET_PARALLEL_PRC <- function(worker_num){
  plan(strategy=multisession, workers=worker_num)
  registerDoFuture()
  print(glue('>> SET PARALLEL PROCESS.'))
}

#(2) Define CALL_API() function
CALL_API_PARALLEL <- function(url, delay_sec=1, verbose=FALSE, ...){
  request_tbl <- list(...) %>% 
    as_tibble()
  return_value = foreach(
    i=1:nrow(x=request_tbl),
    .package=c("tidyverse", "glue"),
    .errorhandling="remove",
    .verbose=verbose) %dorng% {
      Sys.sleep(time=delay_sec)
      GET(url=url, query=request_tbl[i, ]) %>% 
        content()
    }
  return(return_value)
}

#(3) Define SHUT_DOWN() function
SHUT_DOWN <- function(){
  plan(strategy=sequential)
  print(glue('>> SHUT DOWN PARALLEL PROCESS. '))
}

################################################################################
#01. READ TABLE AND EDA
#(1) Read table
api_request_raw <- read_xlsx(path=read_file_path)

#(2) Preprocess ownerNm Column
api_request <- api_request_raw %>% 
  distinct(ClientNm_raw, VhrNo, .keep_all=TRUE) %>% 
  mutate(ownerNm=str_split(string=ownerNm_raw, pattern="\\(", simplify=TRUE)[, 1]) %>% 
  relocate(ownerNm, .after=ownerNm_raw)

#(3) Check unique and missing value
api_request %>% 
  diagnose()

################################################################################
#02. CALL API AND EXTRACT RESULT CODE
#(1) Set parallel process
SET_PARALLEL_PRC()

#(2) Call API
api_return <- CALL_API_PARALLEL(
  url=read_lines(file=api_endpoint),
  kindOf=kind_of,
  token=token,
  ownerNm=api_request[["ownerNm"]],
  vhrNo=api_request[["vhrNo"]]
)

#(3) Extract result code
api_return_extract <- tibble()
for(i in 1:length(x=api_return)){
  temp <- tibble(
    ownerNm    = glue('api_return[[i]]{ownerNm_path}'),
    vhrNo      = glue('api_return[[i]]{vhrNo_path}'),
    resultCode = api_return[[i]][["resultCode"]],
    resultMsg  = api_return[[i]][["resultMsg"]]
  )
  api_return_extract <- bind_rows(api_return_extract, temp)
}

################################################################################
#03. SAVE TABLE
#(1) Define variable
TODAY <- Sys.Date() %>% 
  str_remove_all(string=., pattern="\\-")

#(2) Make directory
if(!dir(path="Output/API_RESULT")){
  dir.create(path="Output/API_RESULT")
}

#(3) Write xlsx format 
write_xlsx(x=api_return_extract, path="Output/API_RESULT/{TODAY}.xlsx")

################################################################################