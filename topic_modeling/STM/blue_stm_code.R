#install.packages("stm")
rm(list=ls()); gc()
library(stm)
library(tidyverse)
library(readxl)
setwd('D:/Summer 2021/교류학술제/STM')

# load data
data <- read_excel("청와대(STM_전처리).xlsx") %>%
  as.data.frame()
processed <- textProcessor(data$text, metadata = data); attach(processed)
out <- prepDocuments(documents, vocab, meta, lower.thresh = 4)


# model search across number of topics
search_k <- searchK(out$documents, out$vocab, K = c(25, 30, 35),
                    prevalence = ~s(day), data= out$meta)
search_k$results

topic_num  #residual을 기준으로 선택
rm(search_k)