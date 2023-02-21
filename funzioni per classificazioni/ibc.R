# function to perform an IBC classification(https://ieeexplore.ieee.org/abstract/document/9333560)
# with the possibility to select automatically the optimal number of nearest neighbours using CV
# input: 
#     - y_tr = outcome train set
#     - x_tr = predictors train set
#     - x_ts = predictors test set
# output:
#     - y_ts = outcome test set

ibc <- function(y_tr, x_tr, x_ts, k_nn = NULL, ...){
  # useful libraries
  lapply(c('purrr', 'FNN'), FUN = function(x){ 
    do.call("require", list(x)) })
  
  # function that calculates the IPDs between a matrix  
  # and a vector via Euclidean distance
  ipd <- function(xtr_cl, xts){
    res <- rep(0, NROW(xtr_cl))
    for(i in 1:NROW(xtr_cl)){
      res[i] <- sqrt(sum((xtr_cl[i,] - xts)^2)) }
    return(res) }
  
  # function to calculate optimal value of 
  # k (number of nearest neighbors) via CV
  choose_ibc_k_nn <- function(data,...){
    set.seed(910)
    data <- data[sample(1:NROW(data)),]
    breichs <- ifelse(NROW(data)<=10,NROW(data),4)
    
    # if n <=10 then it doesn't apply the filtering
    if(breichs == 4){ ks <- c(3,5,7,9) }
    else{ return(NULL) }
    
    folds <- cut(seq(1,NROW(data)), breaks = breichs, labels = FALSE)
    accu <- rep(0,breichs)
    for(i in 1:breichs){
      testIndexes <- which(folds==i,arr.ind=TRUE)
      dbts <- data[testIndexes, ]
      dbtr <- data[-testIndexes, ]
      accu[i] <- confusionMatrix(as.factor(ibc(y_tr=dbtr[,1],x_tr=dbtr[,-1],x_ts=dbts[,-1],
                                               k_nn = ks[i],...)), dbts[,1])$overall[1] }
    return(ks[which.max(accu)]) }
  
  # useful object for the function
  cond_dim <- (NCOL(x_tr) == 1)
  n_ts <- ifelse(cond_dim, length(x_ts), NROW(x_ts))
  y_tr <- as.factor(y_tr);  nomi_cl <- levels(y_tr)
  cond_dim <- (NCOL(x_tr) == 1)
  x_tr <- data.frame(map(x_tr,as.numeric))
  x_ts <- data.frame(map(x_ts,as.numeric))
  tvals <- rep(0, 2);  prev <- rep(0, n_ts)
  
  # choosing k via CV
  if(k_nn == 'CV'){ 
    k_nn <- suppressWarnings(choose_ibc_k_nn(cbind(y_tr, x_tr),...)) }
  
  # cycle on test set observations
  for(i in 1:n_ts){
    if(cond_dim){ xtsi <- x_ts[i] }
    else{ xtsi <- x_ts[i,] }
    
    # without the filtering the observations via knn
    if(is.null(k_nn)){ 
      dbtr <- data.frame(yy = y_tr, x_tr)
      dist1 <- ipd(dbtr[dbtr$yy==nomi_cl[1],-1],xtsi)
      dist2 <- ipd(dbtr[dbtr$yy==nomi_cl[2],-1],xtsi) }
    
    # with the filtering of the observations knn
    else{ 
      dbtr <- data.frame(yy = y_tr, x_tr)
      dd1 <- dbtr[dbtr$yy == nomi_cl[1],]
      dd2 <- dbtr[dbtr$yy == nomi_cl[2],]
      k1 <- attr(knn(train=dd1[,-1],test=xtsi,
                     cl=dd1[,1],k=k_nn),'nn.index')[1,]
      k2 <- attr(knn(train=dd2[,-1],test=xtsi,
                     cl=dd2[,1],k=k_nn),'nn.index')[1,]
      if(cond_dim){ 
        dbknn <- data.frame(yy = c(dd1[k1,1],dd2[k2,1]), 
                            c(dd1[k1,-1],dd2[k2,-1])) }
      if(!cond_dim){ 
        dbknn <- data.frame(yy = c(dd1[k1,1],dd2[k2,1]), 
                            rbind(dd1[k1,-1],dd2[k2,-1]))
        dist1 <- ipd(dbknn[dbknn$yy==1,-1],xtsi)
        dist2 <- ipd(dbknn[dbknn$yy==2,-1],xtsi) }}
    # first two steps of the algorithm
    p1 <- wilcox.test(dist1,dist2,alternative='less')$p.value
    p2 <- wilcox.test(dist1,dist2,alternative='greater')$p.value
    # terzo passo dell'algoritmo
    prev[i] <- ifelse(p1 < p2, nomi_cl[1], nomi_cl[2]) }
  return(prev) }