# UCI_MACHINE_LEARNING_REPOSITORY::Trains Data Set

# y = west / east

library(tidyverse)
source('funzioni per classificazioni/class_bin_nested_cv.R')

db <- read.table('esempi_pratici/data/trains.txt',header=FALSE)

dd <- data.frame(y=as.factor(db[,33]),data.frame(map(db[,c(1:12,23,24,26:29,31)],as.factor))) 
# non normalizzo le esplicative perché son tutte fattori




clbin_nestkfold(dd,'rf') # 0.8
clbin_nestkfold(dd,'knn') # 0.1
clbin_nestkfold(dd,'elasticnet') # 0.7
clbin_nestkfold(dd,'svm') # 0.8
clbin_nestkfold(dd,'ibc') # 0.6
clbin_nestkfold(dd,'tbc') # 0.9
clbin_nestkfold(dbb,'sda') # 0.7

