# UCI_MACHINE_LEARNING_REPOSITORY::Lung_Cancer

# y = {1,2,3}: vari tipi di cancro ai polmoni

# The data described 3 types of pathological lung cancers. 
# The Authors give no information on the individual variables nor on where the data was originally used.


source('funzioni per classificazioni/class_bin_nested_cv.R')
library(tidyverse)

db <- read.table('esempi_pratici/data/lung.txt',header=FALSE,sep=',') %>%
  mutate_all(as.factor) %>% dplyr::rename('y' = V1)

# non normalizzo le esplicative perchè son tutte fattori

# 2 vs 3
dd <- db[db$y != 1,]
# imputazione dati mancanti
dd[which(dd$V39 == '?'),'V39'] <- round(median(as.numeric(dd[which(dd$V39 != '?'),'V39'])))
dd[which(dd$V5 == '?'),'V5'] <- 2
dd <- droplevels(dd)
# 23 obs (13/10), 56 espl


# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(dd,'rf') # 0.6521739
clbin_nestkfold(dd,'knn') # 0.6086957
clbin_nestkfold(dd,'elasticnet') # 0.6086957
clbin_nestkfold(dd,'svm') # 0.6521739
clbin_nestkfold(dd,'ibc') # 0.6956522
clbin_nestkfold(dd,'tbc') # 0.8695652
clbin_nestkfold(dd,'sda') # 0.7391304

##################---------------##################


#####--------#####                     
isnot.const <- function(x) ifelse(length(unique(x)) > 1, TRUE, FALSE)
#####--------#####                    

# 1 vs 2
dd <- droplevels(db[db$y != 3,]) %>% select_if(isnot.const)
dd[which(dd$V5 == '?'),'V5'] <- 1
dd <- droplevels(dd)
# 22 obs (9/13), 52 espl


# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(dd,'rf') # 0.5
clbin_nestkfold(dd,'knn') # 0.5
clbin_nestkfold(dd,'elasticnet') # 0.5454545
clbin_nestkfold(dd,'svm') # 0.5
clbin_nestkfold(dd,'ibc') # 0.7272727 
clbin_nestkfold(dd,'tbc') # 0.8181818

##################---------------##################


# 1 vs 3
dd <- droplevels(db[db$y != 2,]) %>% select_if(isnot.const)
dd[which(dd$V39 == '?'),'V39'] <- 2
dd[which(dd$V5 == '?'),'V5'] <- 1
dd <- droplevels(dd)
# 19 obs (9/10), 55 espl


# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(dd,'rf') # 0.8947368
clbin_nestkfold(dd,'knn') # 0.8947368
clbin_nestkfold(dd,'elasticnet') # 0.8947368 
clbin_nestkfold(dd,'svm') # 0.9473684
clbin_nestkfold(dd,'ibc') # 0.7894737
clbin_nestkfold(dd,'tbc') # 0.8947368



