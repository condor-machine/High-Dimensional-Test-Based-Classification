
source('funzioni per classificazioni/class_bin_nested_cv.R')

library(tidyverse)

# il dataset non è presente in questa repository perché è troppo pesante.
# è comunque reperibile su kaggle (https://www.kaggle.com/code/khushim10/analysis-of-gene-expression-in-brain-cancer/data)
db <- readr::read_csv('esempi_pratici/data/brain.csv')

db1 <- droplevels(db[which(db$type %in% c('glioblastoma','medulloblastoma')),-1])
# 56 obs (34/22)

dd <- data.frame(db1[,1],data.frame(map(db1[,-1],as.numeric)))
# inizialmente tengo i 5000 geni più espressi
dbb <- cbind(dd[,1],dd[,names(sort(colSums(dd[,-1]),decreasing=T)[1:5e3])])

rm(db1)


# ora cerco i geni più espressi, 
# che verranno poi utilizzati come predittori
ord_genes <- sort(colSums(dbb[,-1]))

# grafico della somma sui campioni delle reads (per ogni gene)
# scelgo come numero di geni 600
# perchè più o meno è ilpunto in cui la curva 
# inizia ad esplodere
plot(ord_genes)
abline(h=ord_genes[4400],col='red',lwd=3)


# tengo i 600 geni più espressi
df <- data.frame(y=as.factor(dbb[,1]),dbb[,names(sort(colSums(abs(dbb[,-1])),decreasing=T)[1:6e2])])

# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(df,'rf') # 0.9642857

# normalizzo le esplicative tra [0, 1]
dbf <- data.frame(y=df$y,data.frame(map(df[-1],function(x) (x-min(x))/(max(x)-min(x)))))

# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(dbf,'knn') # 0.9642857
clbin_nestkfold(dbf,'elasticnet') # 0.9642857
clbin_nestkfold(dbf,'svm') # 0.9642857
clbin_nestkfold(dbf,'tbc') # 0.9642857
clbin_nestkfold(dbf,'ibc') # 0.9642857
clbin_nestkfold(dbf,'sda') # 0.9642857


##################---------------##################



db1 <- droplevels(db[which(db$type %in% c('ependymoma','pilocytic_astrocytoma')),-1])
# 61 obs (46/15)

dd <- data.frame(db1[,1],data.frame(map(db1[,-1],as.numeric)))
# inizialmente tengo i 5000 geni più espressi
dbb <- cbind(dd[,1],dd[,names(sort(colSums(dd[,-1]),decreasing=T)[1:5e3])])

rm(db1)

# ora cerco i geni più espressi, 
# che verranno poi utilizzati come predittori
ord_genes <- sort(colSums(dbb[,-1]))

# grafico della somma sui campioni delle reads (per ogni gene)
# scelgo come numero di geni 600
# perchè più o meno è ilpunto in cui la curva 
# inizia ad esplodere...
# potrebbe andare ugualmente bene 800 o 1000
# ma con 600 risparmio un po' di tempo
plot(ord_genes)
abline(h=ord_genes[4400],col='red',lwd=3)


# tengo i 600 geni più espressi
df <- data.frame(y=as.factor(dbb[,1]),dbb[,names(sort(colSums(abs(dbb[,-1])),decreasing=T)[1:600])])


# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(df,'rf') # 0.9180328

# normalizzo le esplicative tra [0, 1]
dbf <- data.frame(y=df$y,data.frame(map(df[-1],function(x) (x-min(x))/(max(x)-min(x)))))


# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(dbf,'knn') # 0.9344262
clbin_nestkfold(dbf,'elasticnet') # 0.9344262
clbin_nestkfold(dbf,'svm') # 0.9344262
clbin_nestkfold(dbf,'tbc') # 0.9508197
clbin_nestkfold(dbf,'ibc') # 0.9344262
clbin_nestkfold(dbf,'sda') # 0.9344262



##################---------------##################


db1 <- droplevels(db[which(db$type %in% c('pilocytic_astrocytoma','glioblastoma')),-1])
# 49 obs (34/15)

dd <- data.frame(db1[,1],data.frame(map(db1[,-1],as.numeric)))

rm(db1)

# inizialmente tengo i 5000 geni più espressi
dbb <- cbind(dd[,1],dd[,names(sort(colSums(dd[,-1]),decreasing=T)[1:5e3])])


# ora cerco i geni più espressi, 
# che verranno poi utilizzati come predittori
ord_genes <- sort(colSums(dbb[,-1]))

# grafico della somma sui campioni delle reads (per ogni gene)
# scelgo come numero di geni 600
# perchè più o meno è ilpunto in cui la curva 
# inizia ad esplodere...
# potrebbe andare ugualmente bene 800 o 1000
# ma con 600 risparmio un po' di tempo
plot(ord_genes)
abline(h=ord_genes[4400],col='red',lwd=3)


# tengo i 600 geni più espressi
df <- data.frame(y=as.factor(dbb[,1]),dbb[,names(sort(colSums(abs(dbb[,-1])),decreasing=T)[1:600])])

# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(df,'rf') # 0.9183673 

# normalizzo le esplicative tra [0, 1]
dbf <- data.frame(y=df$y,data.frame(map(df[-1],function(x) (x-min(x))/(max(x)-min(x)))))


# calcolo accuracy  con 10 - fold - CV
clbin_nestkfold(dbf,'knn') # 0.755102
clbin_nestkfold(dbf,'elasticnet') # 0.877551 
clbin_nestkfold(dbf,'svm') # 0.9387755
clbin_nestkfold(dbf,'tbc') # 0.755102
clbin_nestkfold(dbf,'ibc') # 0.877551
clbin_nestkfold(dbf,'sda') # 0.877551

