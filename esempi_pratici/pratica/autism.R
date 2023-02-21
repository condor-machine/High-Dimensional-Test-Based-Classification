library(edgeR)
library(sva)
library(tidyverse)
library(SummarizedExperiment)
source('funzioni per classificazioni/class_bin_nested_cv.R')

sum_exp <- readRDS('esempi_pratici/data/07_final_SE')

# I  keep only the most highly expressed genes, which are 8895: 
sum_exp <- sum_exp[rowMeans(assay(sum_exp))>=1e5,]
sum_exp

# TMM normalization
assay(sum_exp,'tmm') <- cpm(assay(sum_exp), lib.size = 
                              calcNormFactors(assay(sum_exp), 
                                              method = "TMM") * colSums(assay(sum_exp)))

# I remove the day effect by treating it as a batch effect
adj <- ComBat(log1p(assay(sum_exp,'tmm')), sum_exp$giorno)


# I build the data.frame for classification
tblm <- data.frame(y=sum_exp$salute,genere=sum_exp$genere,
                   famiglia=sum_exp$famiglia,t(exp(adj)))

tblm$y <- relevel(tblm$y,ref='malato')

plot(sort(colSums(tblm[,-(1:3)])))
abline(h=sort(colSums(abs(tblm[,-(1:3)])),decreasing=T)[3e2],col='red',lwd=5)


dbb <- data.frame(tblm[,1:3],tblm[,names(sort(colSums(abs(tblm[,-c(1,2,3)])),decreasing=T)[1:3e2])])

# normalizzo le esplicative tra [0, 1]
dbf <- data.frame(y=dbb[,1],data.frame(map(dbb[,-c(1,2,3)],function(x) (x-min(x))/(max(x)-min(x)))))

clbin_nestkfold(dbf,'rf') # 0.6363636
clbin_nestkfold(dbf,'knn') # 0.7272727
clbin_nestkfold(dbf,'elasticnet') # 0.7272727 
clbin_nestkfold(dbf,'svm') # 0.7727273
clbin_nestkfold(dbf,'htbc',type='ibc') # 0.6363636

clbin_nestkfold(dbf,'sda') # 0.7272727 
clbin_nestkfold(dbf,'tbc') # 0.8636363











