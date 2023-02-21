# UCI_MACHINE_LEARNING_REPOSITORY::Caesarian Section Classification Dataset Data Set

source('funzioni per classificazioni/class_bin_nested_cv.R')

db <- read.csv('esempi_pratici/data/caesarian.csv',header=FALSE)
dd <- data.frame(y=as.factor(db[,6]),db[,1],as.factor(db[,2]),
                 as.factor(db[,3]),as.factor(db[,4]),as.factor(db[,5]))


# in questo dataset non normalizzo le espicative perchè sono tutte fattori tranne una

# calcolo accuracy con 10 - fold - CV
clbin_nestkfold(dd,'rf') # 0.525
clbin_nestkfold(dd,'knn') # 0.65
clbin_nestkfold(dd,'elasticnet') # 0.6375
clbin_nestkfold(dd,'svm') # 0.5875
clbin_nestkfold(dd,'lda') # 0.625
clbin_nestkfold(dd,'tbc') # 0.6625
clbin_nestkfold(dd,'ibc') # 0.6125



