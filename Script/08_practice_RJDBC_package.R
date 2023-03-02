################################################################################
# TITLE        :: PRACTICE RJDBC PACKAGE
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2022-12-14 (WED)
# UPDATED DATE :: 2023-01-29 (SUN)

################################################################################
#00-1. SET WORK ENVIRONMNET
#(1) Import packages
library(tidyverse)
library(dbplyr)
library(rJava)
library(DBI)
library(RJDBC)
library(writexl)

#(2) Set system options
options(scipen=100)

#(3) Define pre-varaible
DB_ADDRESS <- read_lines(file="Key/DB_ADDRESS.txt")
ID <- read_lines(file="Key/DB_ID.txt")
PASSWORD <- read_lines(file="Key/DB_PASSWORD.txt")

################################################################################
#00-2. ASSIGN USER-DEFINED FUNCTION
#(1) connect_oracleDB() function
connect_oracleDB <- function(id, pwd){
  ORACLE_DRIVER <<- JDBC(driverClass="oracle.jdbc.driver.OracleDriver",
                         classPath="c:/Oracle/instantclient_21_7/ojdbc8.jar")
  ORACLE_CONNECTION <<- dbConnect(drv=ORACLE_DRIVER, DB_ADDRESS, id, pwd)
}

#(2) getQuery_oracleDB() function
getQuery_oracleDB <- function(connection=ORACLE_CONNECTION, query){
  return_value <- dbGetQuery(con=ORACLE_CONNECT, 
                             statement=query) %>% 
    as_tibble()
  return(return_value)
}

################################################################################
#01. CONNECT AND GET QUERY
#(1) Connect oracle DB
connect_oracleDB(id=ID, pwd=PASSWORD)

#(2) Check all tables
getQuery_oracleDB(query="
  SELECT * 
  FROM ALL_TABLES 
  WHERE OWNER ='SCRAP' 
    AND TABLE_NAME LIKE 'TB_AT%'
    AND TABLESPACE_NAME LIKE 'TSD_AUTO%'
")

#(3) Extract table
SpecCode_raw <- getQuery_oracleDB(query="
  SELECT *
  FROM TB_AT_SPECCD
")

################################################################################
#02. WRITE TABLE
#(1) Define variables
TODAY <- Sys.Date() %>% 
  str_replace_all(string=., pattern="\\-", replacement="")

#(2) write tsv format
write_tsv(
  x=SPECCD_raw,
  file=glue("Output/SpecCode_raw_{TODAY}.tsv"),
  quote="all"
)

################################################################################