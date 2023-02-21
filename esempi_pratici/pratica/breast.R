
# dataset (kaggle?) con 151 righe e 54677 colonne
# la risposta ha 6 livelli: qui verranno utilizzate
# le coppie di liivelli (per la classific. binaria)
# che sono meno separate


source('funzioni per classificazioni/class_bin_nested_cv.R')
library(tidyverse)

db <- readr::read_csv('esempi_pratici/data/breast.csv')


# luminal_A vs luminal_B: 59 obs (29/30)
dbb <- db[which(db$type %in% c('luminal_A','luminal_B')),-1]


# inizialmente tengo i 5000 geni più espressi
dbb <- data.frame(y=as_factor(dbb[,1]),dbb[,names(sort(colSums(abs(dbb[,-1])),decreasing=T)[1:5e3])])

# ora cerco i geni più espressi, 
# che verranno poi utilizzati come predittori
ord_genes <- sort(colSums(dbb[,-1]))

# grafico della somma sui campioni delle reads (per ogni gene)
# tenere 600 geni sembra poter andare bene
plot(ord_genes)
abline(h=ord_genes[4400],col='red',lwd=3)


# tengo i 600 geni più espressi
df <- data.frame(y=as.factor(dbb[,1]),dbb[,names(sort(colSums(abs(dbb[,-1])),decreasing=T)[1:600])])

# calcolo accuracy  con 10 - fold - CV
perf_kfold(df,'rf') # 0.8983051 

dbf <- data.frame(y=df[,1],data.frame(map(df[,-1],function(y) {
  x = as.numeric(y); xm = (x-min(x))/(max(x)-min(x)); xm })))

clbin_nestkfold(dbf,'knn') # 0.8305085 
clbin_nestkfold(dbf,'elasticnet') # 0.8983051
clbin_nestkfold(dbf,'svm') # 0.9322034 
clbin_nestkfold(dbf,'ibc') # 0.8474576
clbin_nestkfold(dbf,'tbc') # 0.8813559
clbin_nestkfold(dbf,'sda') # 0.8983051

##################---------------##################



# basal vs HER

dbb <- db[which(db$type %in% c('basal','HER')),-1]
# 71 obs (41/30)

# inizialmente tengo i 5000 geni più espressi
dbb <- data.frame(y=as_factor(dbb[,1]),dbb[,names(sort(colSums(abs(dbb[,-1])),decreasing=T)[1:5e3])])
names(dbb[,1]) <- 'y'

# ora cerco i geni più espressi, 
# che verranno poi utilizzati come predittori
ord_genes <- sort(colSums(dbb[,-1]))

# grafico della somma sui campioni delle reads (per ogni gene)
# tenere 600 geni sembra poter andare bene
plot(ord_genes)
abline(h=ord_genes[4400],col='red',lwd=3)


# tengo i 600 geni più espressi
df <- data.frame(y=as.factor(dbb[,1]),dbb[,names(sort(colSums(abs(dbb[,-1])),decreasing=T)[1:600])])


# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(df,'rf') # 0.915493

dbf <- data.frame(y=df[,1],data.frame(map(df[,-1],function(y) {
  x = as.numeric(y); xm = (x-min(x))/(max(x)-min(x)); xm })))

clbin_nestkfold(dbf,'knn') # 0.7887324
clbin_nestkfold(dbf,'elasticnet') # 0.9014085
clbin_nestkfold(dbf,'svm') # 0.915493
clbin_nestkfold(dbf,'ibc') # 0.8309859 
clbin_nestkfold(dbf,'tbc') # 0.8873239 
clbin_nestkfold(dbf,'sda') # 0.915493


