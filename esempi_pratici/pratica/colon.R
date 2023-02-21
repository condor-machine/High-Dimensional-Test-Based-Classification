# Kaggle::Colon Cancer Gene Expression Data

# y = Normal/Abnormal
# x = 2001 genes

source('funzioni per classificazioni/class_bin_nested_cv.R')
library(tidyverse)

db <- readr::read_csv('esempi_pratici/data/colon.csv')
dd <- cbind(db[,2002],db[,2:2001])


# grafico della somma delle reads sui campioni
# per scegliere quanti geni tenere
plot(sort(colSums(abs(dd[,-1]))))
abline(h=sort(colSums(abs(dd[,-1])),decreasing=T)[2e2],col='red',lwd=5)


# 200 geni più espressi
bd <- data.frame(y=as.factor(dd[,1]),dd[,names(sort(colSums(abs(dd[,-1])),decreasing=T)[1:2e2])])

perf_kfold(bd,'rf') # 0.8225806

# normalizzo le esplicative tra [0, 1]
dbf <- data.frame(y=bd$y,data.frame(map(bd[-1],function(x) (x-min(x))/(max(x)-min(x)))))

# calcolo accuracy e F1_score con 10 - fold - CV
clbin_nestkfold(dbf,'knn') # 0.7741935
clbin_nestkfold(dbf,'elasticnet') # 0.8225806
clbin_nestkfold(dbf,'svm') # 0.8064516
clbin_nestkfold(dbf,'htbc',type='ibc') # 0.8064516  
clbin_nestkfold(dbf,'htbc', type = 'tbcv') # 0.8548387 
clbin_nestkfold(dbf,'sda') # 7903226












