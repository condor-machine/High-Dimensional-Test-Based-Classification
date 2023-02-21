# UCI_MACHINE_LEARNING_REPOSITORY::gastro

# This dataset contains the features extracted from a database of colonoscopic videos 
# showing gastrointestinal lesions. There are features vectors for 76 lesions, 
# and there are 3 types of lesion: hyperplasic, adenoma and serrated adenoma.
# It is possible to consider this classification problem as a binary one by combining adenoma and 
# serrated adenoma in the same class. According to this, hyperplasic lesions would belong to the class 
# 'benign' while the other two types of gastrointestinal lesions would go to the 'malignant' class.

# y = benign (hyperplasic lesions) /  malign (others)

library(tidyverse)
source('funzioni per classificazioni/class_bin_nested_cv.R')


db <- data.frame(t(read.table('esempi_pratici/data/gastro.txt',header=TRUE,sep=',')))[,-1]


is_const <- function(x){
  if(length(unique(x)) == 1) return(TRUE)
  return(FALSE) }


# Every lesion appears twice because it has been recorded using two types of lights:

# luce tipo 1
ddl1 <- db[seq(1,152,by=2),]
dd1 <- ddl1 %>% dplyr::select(!where(is_const)) 
names(dd1)[1] <- 'y'


# luce tipo 2
ddl2 <- db[seq(2,152,by=2),]
dd2 <- ddl2 %>% dplyr::select(!where(is_const)) 
names(dd2)[1] <- 'y'


# unione luci per colonna perchè le misurazioni riguardano lo stesso soggetto
dbb <- cbind(dd1,dd2[,-1])
names(dbb)[-1] <- paste('X',(2:932),sep='')


dbb <- data.frame(y=as.factor(dbb$y),data.frame(map(dbb[,-1],as.numeric)))


clbin_nestkfold(dbb,'rf') # 0.9342105

# normalizzo le esplicative tra [0, 1]
dbf <- data.frame(y=dbb$y,data.frame(map(dbb[,-1],
                                         function(x) (x-min(x))/(max(x)-min(x)))))


clbin_nestkfold(dbf,'sda') #  0.9210526
clbin_nestkfold(dbf,'tbc') # 0.9473684
clbin_nestkfold(dbf,'ibc') # 0.9473684
clbin_nestkfold(dbf,'elasticnet') # 0.961
clbin_nestkfold(dbf,'svm') # 0.908
clbin_nestkfold(dbf,'knn') # 0.789




