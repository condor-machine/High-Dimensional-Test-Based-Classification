# UCL:: Connectionist Bench (Sonar, Mines vs. Rocks) Data Set


source('funzioni per classificazioni/class_bin_nested_cv.R')

db <- read.table('esempi_pratici/data/sonar.txt',header=F,sep=',')
dbb <- data.frame(y = as.factor(db[,61]),db[,-61])

clbin_nestkfold(dbb,'rf') # 0.87


dbf <- data.frame(y=dbb$y,data.frame(map(dbb[,-1],function(x) (x-min(x))/(max(x)-min(x)))))


clbin_nestkfold(dbf,'knn') # 0.817 
clbin_nestkfold(dbf,'elasticnet') # 0.7548077
clbin_nestkfold(dbf,'svm') # 0.75
clbin_nestkfold(dbf,'lda') # 0.7163462 
clbin_nestkfold(dbf,'tbc') # 0.7067308
clbin_nestkfold(dbf,'ibc') # 0.8461538
