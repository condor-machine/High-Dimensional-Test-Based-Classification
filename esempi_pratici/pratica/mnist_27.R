# dslabs::mnist_27

# y = 2 / 7

# We only include a randomly selected set of 2s and 7s along with the 
# two predictors based on the proportion of dark pixels in the upper left and 
# lower right quadrants respectively. The dataset is divided into training and test sets

source('funzioni per classificazioni/class_bin_nested_cv.R')
library(dslabs)

dbtr <- data.frame(y=as.factor(mnist_27$train$y),mnist_27$train[,-1])
dbts <- data.frame(y=as.factor(mnist_27$test$y),mnist_27$test[,-1])

db <- rbind(dbtr, dbts)

# non normalizzo i predittori perchè entrambi rappresentano
# la proporzione di pixel neri (in due quadranti diversi)

clbin_nestkfold(db,'rf') #  0.826



# calcolo accuracy e F1_score con 10 - fold - CV
clbin_nestkfold(db,'knn') # 0.832
clbin_nestkfold(db,'elasticnet') # 0.783
clbin_nestkfold(db,'svm') # 0.79
clbin_nestkfold(db,'lda') # 0.792
clbin_nestkfold(db,'tbc') # 0.789
clbin_nestkfold(db,'ibc') # 0.83