# codice per IBC (https://ieeexplore.ieee.org/abstract/document/9333560)
# con selezione automatica del numero ottimale di nearest neighbours tramite CV
# input: 
#     - y_tr = risposta train set
#     - x_tr = predittori train set
#     - x_ts = predittori test set
# output:
#     - y_ts = risposta test set

ibc <- function(y_tr, x_tr, x_ts, k_nn = NULL, ...){
  # librerie richieste
  lapply(c('purrr', 'FNN'), FUN = function(x){ 
    do.call("require", list(x)) })
  
  # funzione che calcola le IPD tra una matrice ed 
  # un vettore tramite distanza euclidea
  ipd <- function(xtr_cl, xts){
    res <- rep(0, NROW(xtr_cl))
    for(i in 1:NROW(xtr_cl)){
      res[i] <- sqrt(sum((xtr_cl[i,] - xts)^2)) }
    return(res) }
  
  # funzione per calcolare valore ottimale di 
  # k (numero di nearest neighbour) tramite CV
  choose_ibc_k_nn <- function(data,...){
    set.seed(910)
    data <- data[sample(1:NROW(data)),]
    breichs <- ifelse(NROW(data)<=10,NROW(data),4)
    
    # se n <=10 allora non filtra
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
  
  # oggetti utili per la funzione
  cond_dim <- (NCOL(x_tr) == 1)
  n_ts <- ifelse(cond_dim, length(x_ts), NROW(x_ts))
  y_tr <- as.factor(y_tr);  nomi_cl <- levels(y_tr)
  cond_dim <- (NCOL(x_tr) == 1)
  x_tr <- data.frame(map(x_tr,as.numeric))
  x_ts <- data.frame(map(x_ts,as.numeric))
  tvals <- rep(0, 2);  prev <- rep(0, n_ts)
  
  # scelta k tramite CV
  if(k_nn == 'CV'){ 
    k_nn <- suppressWarnings(choose_ibc_k_nn(cbind(y_tr, x_tr),...)) }
  
  # ciclo sulle osservazioni del test set
  for(i in 1:n_ts){
    if(cond_dim){ xtsi <- x_ts[i] }
    else{ xtsi <- x_ts[i,] }
    
    # senza filtraggio osservazioni tramite knn
    if(is.null(k_nn)){ 
      dbtr <- data.frame(yy = y_tr, x_tr)
      dist1 <- ipd(dbtr[dbtr$yy==nomi_cl[1],-1],xtsi)
      dist2 <- ipd(dbtr[dbtr$yy==nomi_cl[2],-1],xtsi) }
    
    # con filtraggio osservazioni tramite knn
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
    # primi due passi dell'algoritmo
    p1 <- wilcox.test(dist1,dist2,alternative='less')$p.value
    p2 <- wilcox.test(dist1,dist2,alternative='greater')$p.value
    # terzo passo dell'algoritmo
    prev[i] <- ifelse(p1 < p2, nomi_cl[1], nomi_cl[2]) }
  return(prev) }
