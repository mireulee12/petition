#install.packages("stm")
rm(list=ls()); gc()
library(stm)
library(tidyverse)
library(readxl)
setwd('D:/Summer 2021/교류학술제/STM')

# load data
data <- read_excel("국회(STM_전처리).xlsx") %>%
  as.data.frame()
processed <- textProcessor(data$text, metadata = data); attach(processed)
out <- prepDocuments(documents, vocab, meta, lower.thresh = 3)


# model search across number of topics
search_k <- searchK(out$documents, out$vocab, K = c(5, 10, 15, 20), 
                   prevalence = ~s(day), data= out$meta)
search_k$results

topic_num = 10 #residual을 기준으로 선택
rm(search_k)

# run stm model
assembly_fit <- stm(documents = out$documents, vocab = out$vocab, 
                       K = topic_num, prevalence = ~s(day), 
                       data = out$meta, init.type = "Spectral")

save(assembly_fit, file = "assembly_fit.rda")

