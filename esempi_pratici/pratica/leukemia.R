# Kaggle::Gene expression dataset (Golub et al.)
# Molecular Classification of Cancer by Gene Expression Monitoring

# Leukemia dataset

# This dataset comes from a proof-of-concept study published in 1999 by Golub et al. 
# It showed how new cases of cancer could be classified by gene expression monitoring 
# (via DNA microarray) and thereby provided a general approach for identifying new cancer
# classes and assigning tumors to known classes. These data were used to classify patients 
# with acute myeloid leukemia (AML) and acute lymphoblastic leukemia (ALL).

# There are two datasets containing the initial (training, 38 samples) and independent 
# (test, 34 samples) datasets used in the paper. These datasets contain measurements
# corresponding to ALL and AML samples from Bone Marrow and Peripheral Blood. 
# Intensity values have been re-scaled such that overall intensities for each chip are equivalent

# Molecular Classification of Cancer: Class Discovery and Class Prediction by Gene Expression
# Science 286:531-537. (1999). Published: 1999.10.14
# T.R. Golub, D.K. Slonim, P. Tamayo, C. Huard, M. Gaasenbeek, J.P. Mesirov, H. Coller, M. Loh, 
# J.R. Downing, M.A. Caligiuri, C.D. Bloomfield, and E.S. Lander

# These datasets are great for classification problems. 
# The original authors used the data to classify the type 
# of cancer in each patient by their gene expressions

# y = ALL/AML
# x = 7129 genes


library(tidyverse)
source('funzioni per classificazioni/class_bin_nested_cv.R')

# PULIZIA DATI TRAIN SET
# dftr : 7129 righe (geni); 78 colonne (2 ID + 76 campioni)
dftr <- read_csv('esempi_pratici/data/leukemia_train.csv')

# i campioni sono duplicati
dftr <- dftr %>% dplyr::select(-contains('call'))  %>% dplyr::select(-`Gene Description`)
dbtr <- data.frame(t(dftr[,-1]))
colnames(dbtr) <- make.names(dftr$`Gene Accession Number`)

# dbtr: 38 righe (campioni); 7130 colonne (1 risposta, 7129 geni)
dbtr <- cbind(y = as.factor(c(rep('ALL',27),rep('AML',11))),dbtr)


# PULIZIA DATI TEST SET
# dfts : 7129 righe (geni); 70 colonne (35 campioni)
dfts <- read_csv('C:/Users/zebra/Desktop/7351/data/leukemia_test.csv')

# i campioni sono duplicati
dfts <- dfts %>% dplyr::select(-contains('call'))  %>% dplyr::select(-`Gene Description`)
dbts <- data.frame(t(dfts[,-1]))
colnames(dbts) <- make.names(dfts$`Gene Accession Number`)

# dbtr: 38 righe (campioni); 7130 colonne (1 risposta, 7129 geni)
dbts <- cbind(y =  as.factor(c(rep('ALL',20),rep('AML',14))),dbts)

# unione dei due database (train/test set)
db <- rbind(dbtr,dbts)

# grafico della somma sui campioni delle reads (per ogni gene)
# scelgo come numero di geni 500
# perchè più o meno è ilpunto in cui la curva 
# inizia ad esplodere...
plot(sort(colSums(abs(db[,-1]))))
abline(h=sort(colSums(abs(db[,-1])),decreasing=T)[5e2],col='red',lwd=5)

# tengo i 500 geni più espressi
df <- data.frame(y=as.factor(db[,1]),db[,names(sort(colSums(abs(db[,-1])),decreasing=T)[1:5e2])])

# calcolo accuracy con 10 - fold - CV
clbin_nestkfold(df,'rf') # 0.9583333

# normalizzo le esplicative tra [0, 1]
dbf <- data.frame(y=df$y,data.frame(map(df[,-1],function(x) (x-min(x))/(max(x)-min(x)))))

# calcolo accuracy con 10 - fold - CV
clbin_nestkfold(dbf,'knn') # 0.9166667 
clbin_nestkfold(dbf,'elasticnet') # 0.9583333
clbin_nestkfold(dbf,'svm') # 0.9722222 
clbin_nestkfold(dbf,'tbc') # 0.986
clbin_nestkfold(dbf,'ibc') # 0.9444444 
clbin_nestkfold(dbf,'sda') # 0.986

