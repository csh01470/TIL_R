################################################################################
# TITLE        :: PRACTICE PREPROCESSING METHOD
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2022-10-07 (FRI)
# UPDATED DATE :: 2023-03-06 (MON)

################################################################################
#00. SET WORK ENVIRONMENT
#(1) Import package
library(tidyverse)

#(2) Set system option
options(scipen=100)

################################################################################
#01. PRACTICE VARIOUS MAP() FUNCTIONS
#(1) Use map_{*}() function
vector_01 <- 1:4 
map_dbl(.x=vector_01, .f=~.x+1)

#(2) Use map2_{*}() function
vector_02 <- c("Data", "Analytics", "With", "R")
map2_chr(.x=vector_01, .y=vector_02, .f=~str_c(.x, "_", .y))

#(3) Use pmap_{*}() function
vector_03 <- c(TRUE, TRUE, FALSE, TRUE)
pmap_chr(.l=list(vector_01, vector_02, vector_03), .f=~str_c(..1, "_", ..2, "_", ..3))

#(4) Use map_df() function
tibble_01 <- tibble(
  var_01=c(17, 23, 4, 10, 11), 
  var_02=c(24, 5, 6, 12, 18), 
  var_03=c(1, 7, 13, 19, 25), 
  var_04=c(8, 14, 20, 21, 2), 
  var_05=c(15, 16, 22, 3, 9)
)
map_df(.x=tibble_01, .f=max)

#(5) Use modify() function 
modify(.x=tibble_01, .f=max) %>% 
  slice(1)
map_df(.x=tibble_01, .f=max)

#PLUS) Use formula format
map_df(.x=tibble_01, .f=~.x+1)

################################################################################
#03. PRACTICE ROWWISE() FUNCTION 
#(1) Use rowwise() function
tibble_01 %>% 
  rowwise() %>% 
  mutate(var_sum=sum(var_01, var_02, var_03, var_04))

#(2) Compare without rowwise() function
tibble_01 %>% 
  mutate(var_sum=sum(var_01, var_02, var_03, var_04))

################################################################################
#04. PRACTICE NESTED AND UNNESTED DATAFRAME
#(1) Use nest() function
tibble_02 <- tribble(
  ~var_01, ~var_02, ~var_03,
  "A", 1, 0,
  "A", 2, -5,
  "B", 3, 2,
  "C", 5, 4  
)
tibble_02 %>% 
  group_by(var_01) %>% 
  nest()

#(2) Use unnest() function
tibble_02 %>% 
  group_by(var_01) %>% 
  nest() %>% 
  unnest(data)
  
################################################################################
#05. PRACTICE PIVOT() function
#(1) Use pivot_longer() function
iris_longer <- datasets::iris %>% 
  mutate(RowNumber=row_number()) %>%
  relocate(RowNumber, .before=Species) %>% 
  pivot_longer(data=., 
               cols=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
               names_to="column_name", 
               values_to="data")
iris_longer

#(2) Use pivot_longer() function
iris_wider <- iris_longer %>% 
  pivot_wider(data=., 
              names_from="column_name",
              values_from="data")
iris_wider

################################################################################
#06. PRACTICE SEPERATE_ROWS() FUNCTION
#(1) Use Seperate_rows() function
tibble_03 <- tibble(
  var_01 = c(1, 2, 3, 4, 5),
  var_02 = c("data_01##data_02##data_03##data_04##data_05",
             "data_06##data_07##data_08##data_09##data_10",
             "data_11##data_12##data_13##data_14##data_15",
             "data_16##data_17##data_18##data_19##data_20",
             "data_21##data_22##data_23##data_24##data_25")
)
tibble_03 %>% 
  separate_rows(data=., var_02, sep="\\##")

#(2) Reverse `tibble_03` object 
tibble_03 %>% 
  separate_rows(data=., var_02, sep="\\##") %>% 
  group_by(var_01) %>% 
  reframe(var_02=str_c(var_02, collapse=", "))

################################################################################