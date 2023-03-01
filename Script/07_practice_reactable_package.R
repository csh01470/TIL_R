################################################################################
# TITLE        :: PRACTICE REACTABLE PACKAGE
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2022-12-08 (FRI)
# UPDATED DATE :: 2023-01-17 (TUE)

################################################################################
#00. SET WORK ENVIRONMENT
#(1) Import packages
library(tidyverse)
library(dlookr)
library(glue)
library(reactable)

#(2) Set system options
options(scipen=100)

################################################################################
#01. READ TABLE AND CHECK QUALITY
#(1) Read table
USEDCAR_train_raw <- read_csv(
  file="/Users/CSH/Workspace/R/Projects/Dacon_usedcar_price/data/raw/train.csv"
)

#(2) Diagnose table
USEDCAR_train_raw %>% diagnose()

################################################################################
#02. PRACTICE REACTABLE
#(1) Define reactable_01
reactable_01 <- reactable(
  data            = USEDCAR_train_raw,
  defaultColDef   = colDef(headerStyle = list(background = "#f7f7f8")),
  columns         = list(id           = colDef(minWidth = 40),
                         title        = colDef(minWidth = 140),
                         odometer     = colDef(minWidth = 65),
                         location     = colDef(minWidth = 90),
                         engine       = colDef(minWidth = 105),
                         transmission = colDef(minWidth = 90),
                         fuel         = colDef(minWidth = 65),
                         paint        = colDef(minWidth = 55),
                         year         = colDef(minWidth = 50),
                         target       = colDef(minWidth = 65)),
  defaultPageSize = 10, 
  minRows         = 10,
  height          = "auto",
  resizable       = TRUE,
  wrap            = FALSE,
  bordered        = TRUE,
  fullWidth       = TRUE,
  style           = list(fontFamily="D2Coding", 
                         fontSize = "12px")
)

#(2) Print reactable_01
print(reactable_01)

################################################################################