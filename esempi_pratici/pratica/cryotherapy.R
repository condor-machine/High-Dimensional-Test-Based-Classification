# UCI_MACHINE_LEARNING_REPOSITORY::cryotherapy


source('funzioni per classificazioni/class_bin_nested_cv.R')

db <- read.table('esempi_pratici/data/cryotherapy.txt')

dd <- droplevels(data.frame(y=as.factor(db[-1,7]),as.factor(db[-1,1]),as.factor(db[-1,5]),db[-1,c(2,3,4,6)]))


clbin_nestkfold(dd,'rf') # 0.8666667

# normalizzo tra [0,1] le esplicative numeriche
dbf <- data.frame(dd[,(1:3)],data.frame(map(dd[,(4:7)],function(x) {
  y = as.numeric(x); xn = (y - min(y))/(max(y) - min(y)); xn })))


clbin_nestkfold(dbf,'knn') # 0.8111111
clbin_nestkfold(dbf,'elasticnet') # 0.7555556
clbin_nestkfold(dbf,'svm') # 0.7888889
clbin_nestkfold(dbf,'lda') # 0.7888889
clbin_nestkfold(dbf,'tbc') # 0.8111111
clbin_nestkfold(dbf,'ibc') # 0.8333333
