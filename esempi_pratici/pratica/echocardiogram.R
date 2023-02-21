# UCI_MACHINE_LEARNING_REPOSITORY::echocardiogram

# y = the patient (will / will_not) survive at least one year 

# All the patients suffered heart attacks at some point in the past. Some are still alive and some are not. 
# The survival and still-alive variables, when taken together, 
# indicate whether a patient survived for at least one year following the heart attack.
# The problem addressed by past researchers was to predict from the other variables
# whether or not the patient will survive at least one year. 
# The most difficult part of this problem is correctly predicting that 
# the patient will NOT survive. (Part of the difficulty seems to be the size of the data set.)



source('funzioni per classificazioni/class_bin_nested_cv.R')

library(tidyverse)

db <- read.table('esempi_pratici/data/echocardiogram.txt',header=FALSE,sep=',')[-50,-1]

dd <- db %>% dplyr::filter(V2 != '?') %>% 
  dplyr::select(-V11) %>% droplevels() %>%
  dplyr::mutate_all(function(x) as.numeric(as.character(x))) %>%
  dplyr::mutate_all(function(x) replace(x, is.na(x), round(mean(x, na.rm = TRUE)))) %>%
  dplyr::mutate_at(vars(V2, V4, V12, V13),~as.factor(.x)) %>% 
  dplyr::mutate(y = V2) %>% dplyr::select(-V2) %>% dplyr::select(c(y,everything()))



# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(dd,'rf') # 0.8320611


# normalizzo le esplicative numeriche tra [0, 1]
dbf <- data.frame(y=dd$y,data.frame(map(dd[,-1],function(x) {
  if(length(unique(x)) > 5) (x-min(x))/(max(x)-min(x))
  else x })))

# summary(dd)
# summary(dbf)

# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(dbf,'knn') # 0.8473282
clbin_nestkfold(dbf,'elasticnet') # 0.8473282 
clbin_nestkfold(dbf,'svm') # 0.8473282
clbin_nestkfold(dbf,'lda') # 0.8396947
clbin_nestkfold(dbf,'tbc') # 0.8244275
clbin_nestkfold(dbf,'ibc') # 0.8320611

