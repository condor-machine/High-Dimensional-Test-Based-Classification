# datasets::iris

# This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters 
# of the variables sepal length and width and petal length and width, respectively, 
# for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.

# tolgo la classe 'setosa' per analizzare le due classi meno separate

# y = versicolor/virginica
# x = 4 espl continue

source('funzioni per classificazioni/class_bin_nested_cv.R')



db <- data.frame(y=droplevels(as.factor(iris[iris$Species != 'setosa',5])),
                 iris[iris$Species != 'setosa',-5])


# calcolo accuracy con 10 - fold - CV
clbin_nestkfold(db,'rf') # 0.93
clbin_nestkfold(db,'knn') # 0.95
clbin_nestkfold(db,'elasticnet') # 0.94
clbin_nestkfold(db,'svm') # 0.95
clbin_nestkfold(db,'lda') # 0.96
clbin_nestkfold(db,'tbc') # 0.96
clbin_nestkfold(db,'ibc') # 0.94


