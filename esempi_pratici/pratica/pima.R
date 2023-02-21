# MASS::(Pima.tr, Pima.te)   (uno dei due dataset utilizzati nell'articolo di riferimento)

# y = diabetes: y / n

# A population of women who were at least 21 years old, 
# of Pima Indian heritage and living near Phoenix, Arizona, 
# was tested for diabetes according to World Health Organization criteria. 
# The data were collected by the US National Institute of Diabetes and 
# Digestive and Kidney Diseases. We used the 532 complete records after dropping 
# the (mainly missing) data on serum insulin

source('funzioni per classificazioni/class_bin_nested_cv.R')

library(MASS)

db <- rbind(Pima.tr,Pima.te)
dbb <- data.frame(y=db$type,db[,-8])

clbin_nestkfold(dbb,'rf') # 0.7744361

# normalizzo le esplicative tra [0, 1]
dbf <- data.frame(y=dbb$y,data.frame(map(dbb[,-1],function(x) (x-min(x))/(max(x)-min(x)))))


# calcolo accuracy e F1_score con 10 - fold - CV
clbin_nestkfold(dbf,'knn') # 0.7424812
clbin_nestkfold(dbf,'elasticnet') # 0.768797
clbin_nestkfold(dbf,'svm') # 0.787594
clbin_nestkfold(dbf,'lda') # 0.7744361 
clbin_nestkfold(dbf,'tbc') # 0.768797
clbin_nestkfold(dbf,'ibc') # 0.768797