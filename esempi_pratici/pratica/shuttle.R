source('funzioni per classificazioni/class_bin_nested_cv.R')

library(MASS)

db <- data.frame(y=shuttle[,7],shuttle[,-7])


clbin_nestkfold(db,'rf') # 0.984375

clbin_nestkfold(db,'knn') #  0.96875 
clbin_nestkfold(db,'elasticnet') # 0.9804688
clbin_nestkfold(db,'svm') # 0.9804688
clbin_nestkfold(db,'lda') # 0.9335938
clbin_nestkfold(db,'htbc', type = 'tbc') # 0.9335938
clbin_nestkfold(db,'htbc',type='ibcv') # 0.96875 
clbin_nestkfold(db,'htbc', type = 'tbcv') #  972


