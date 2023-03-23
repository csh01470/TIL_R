################################################################################
# TITLE        :: PRACTICE SOURCE FUNCTION
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2022-11-11 (FRI)
# UPDATED DATE :: 2023-01-07 (SAT)

################################################################################
#00. SET WORK ENVIRONMNET
#(1) Import packages
library(tidyverse)
library(glue)

#(2) Define pre-variable
DIR_PATH <- "Script/"
KEYWORD <- "object"

################################################################################
#01. RUN SCRIPTS
#(1) Define variable
SCRIPT_LIST <- dir(path=DIR_PATH) %>% 
  str_extract_all(string=., pattern=glue("[A-Za-z0-9_.]+{KEYWORD}[A-Za-z0-9_.]+"), simplify=TRUE) %>% 
  subset(x=., subset=(.!=""))

#(2) Run scripts
for(i in 1:length(x=SCRIPT_LIST)){
  source(file=glue('{DIR_PATH}{SCRIPT_LIST[i]}'))
  print(glue('>> WRITE DONE. SCRIPT NAME IS "{SCRIPT_LIST[i]}".'))
}

################################################################################