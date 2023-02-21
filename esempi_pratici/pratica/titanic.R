library(tidyverse)

db <- read.csv('esempi_pratici/data/09_titanic.csv') %>%
  dplyr::select(-c(PassengerId,Name,Ticket,Cabin)) %>%
  dplyr::mutate(Parch = factor(Parch)) %>% 
  dplyr::mutate(SibSp = factor(SibSp)) %>%
  dplyr::mutate(Pclass = factor(Pclass)) %>%
  dplyr::rename('y' = Survived) %>% na.omit() %>% 
  dplyr::mutate_all(as.numeric) %>%  dplyr::mutate(y = factor(y))


source('funzioni per classificazioni/class_bin_nested_cv.R')



clbin_nestkfold(db,'rf') # 0.8137255


dbf <- data.frame(y=db$y,data.frame(map(db[,-1],function(x) (x-min(x))/(max(x)-min(x)))))


# calcolo accuracy e F1_score con 10 - fold - CV
clbin_nestkfold(dbf,'knn') # 0.7759104
clbin_nestkfold(dbf,'elasticnet') # 0.7843137
clbin_nestkfold(dbf,'svm') # 0.780112
clbin_nestkfold(dbf,'lda') # 0.7885154
clbin_nestkfold(dbf,'tbc') # 0.7815126
clbin_nestkfold(dbf,'ibc') # 0.7955182 

