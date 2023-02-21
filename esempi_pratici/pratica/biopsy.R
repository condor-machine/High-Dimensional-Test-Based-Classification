# MASS::biopsy (uno dei due dataset analizzati nell'articolo di riferimento)

# y = class: "benign" or "malignant".

# This breast cancer database was obtained from the University of Wisconsin Hospitals, 
# Madison from Dr. William H. Wolberg. He assessed biopsies of breast tumours for 699 patients 
# up to 15 July 1992; each of nine attributes has been scored on a scale of 1 to 10, 
# and the outcome is also known. There are 699 rows and 11 columns.

# in questo dataset non normalizzo le espicative perchè sono già tutte sulla stessa scala

source('funzioni per classificazioni/class_bin_nested_cv.R')
library(MASS)
library(tidyverse)

db <- na.omit(data.frame(y = biopsy[,11],biopsy[,-c(1,11)]))


# calcolo accuracy con 10 - fold - CV
clbin_nestkfold(db,'rf') # 0.9707174 
clbin_nestkfold(db,'knn') # 0.9692533 
clbin_nestkfold(db,'elasticnet') # 0.9692533
clbin_nestkfold(db,'svm') # 0.9707174 
clbin_nestkfold(db,'lda') # 0.9604685 
clbin_nestkfold(db,'tbc') # 0.9604685
clbin_nestkfold(db,'ibc') # 0.9707174 








