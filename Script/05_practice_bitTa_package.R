################################################################################
# TITLE        :: PRACTICE BITTA(KOREAN-NLP) PACKAGE
# AUTHOR       :: CHOI SANG HEON
# CREATED DATE :: 2023-01-02 (MON)
# UPDATED DATE :: 2023-01-15 (SUN)

################################################################################
#00-1. SET WORK ENVIRONMENT 
#(1) Import packages
library(tidyverse)
library(glue)
library(bitTA)

#(2) Set options
options(scipen=100)

################################################################################
#00-1. ASSIGN USER-DEFINED FUNCTION
#(1) NGRAM_SIMILARITY() function
NGRAM_SIMILARITY <- function(vector_01, vector_02){
  count = 0
  for(token in vector_01){
    if(token %in% vector_02){
      count = count + 1
    }
  }
  return_value = count/length(x=vector_01)
  return(return_value)
}

################################################################################
#01. PRACTICE GET_SPACING() FUNCTION
#(1) Define Variables 
CHR_01 <- "
  최근음성인식정확도가높아짐에따라많은음성데이터가텍스트로변환되고분석되기시작했는데,
  이를위해잘동작하는띄어쓰기엔진은거의필수적인게되어버렸다
"
CHR_02 <- "
  글쓰기에서맞춤법과띄어쓰기를올바르게하는것은좋은글이될수있는요건중하나이다.
  하지만요즘학생들은부족한어문규정지식으로인해맞춤법과띄어쓰기에서많은오류를범하기도한다.
  본연구는그중띄어쓰기가글을인식하는데중요한역할을하는것으로판단하여,
  대학생들이띄어쓰기에대해서어느정도정확하게인식하고있는지,
  실제오류실태는어떠한지에대해살펴서그오류를개선할수있는교육방안을마련할필요가있다고판단하였다.
"

#(2) Use get_spacing() function
get_spacing(x=CHR_01)
get_spacing(x=CHR_02)

################################################################################
#02. PRACTICE MORPHO_MECAB() FUNCTION
#(1) Define documents(vector)
DOCS_01 <- c(
  "님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.",
  "푸른 산빛을 깨치고 단풍나무 숲을 향하여 난 작은 길을 걸어서, 차마 떨치고 갔습니다."
)
DOCS_02 <- c(
  "은전한닢 형태소분석기 시스템과 사전은 bitTA 패키지의 비네트인 Install mecab-ko에 설명되어 있습니다.",
  "사전에 설치해야 하는 리소스는 다음의 순서와 방법대로 설치하는 것을 추천합니다."
)

#(2) Use morpho_mecab() function
morpho_mecab(x=DOCS_01, type="morpheme")
morpho_mecab(x=DOCS_02, type="morpheme")

################################################################################
#03. PRACTICE NGRAM_SIMILARITY() FUNCTION
#(1) Define variables
PREDICT_raw <- c(
  "1.6 터보 모던", "1.6 터보 모던 코어", "1.6 터보 스포츠", "1.6 터보 스포츠 코어",
  "1.6 터보 JBL 익스트림 사운드 에디션", "1.6 터보 익스트림 드라이빙 에디션" 
)
TRUTH_raw <- c("THE ALL NEW 벨로스터 1.6[가솔린]터보 스포츠 코어")

#(2) Decompose morpheme
PREDICT <- str_to_lower(string=PREDICT_raw) %>% 
  morpho_mecab(x=., type="noun2")
TRUTH <- str_to_lower(string=TRUTH_raw) %>% 
  morpho_mecab(x=., type="noun2")

#(3) Calculate text-similarity by unigram
TS_LIST = c()
for(i in 1:length(x=PREDICT)){
  TS <- NGRAM_SIMILARITY(vector_01=TRUTH, vector_02=PREDICT[[i]])
  TS_LIST <- c(TS_LIST, TS)
}

#(4) Check text-similarity
CHK_TS_TBL <- tibble(
  PREDICT  = PREDICT_raw,
  TRUTH    = TRUTH_raw,
  TS       = TS_LIST
) %>% 
  arrange(desc(TS))

#(5) Print table
CHK_TS_TBL

################################################################################