# codice per TBC e HDTBC (in automatico se p > n)
# input: 
#     - y_tr = risposta train set
#     - x_tr = predittori train set
#     - x_ts = predittori test set
# output:
#     - y_ts = risposta test set

tbc <- function(y_tr, x_tr, x_ts){
  # librerie richieste
  lapply(c('purrr', 'Hotelling'), FUN = function(x){ 
    do.call("require", list(x)) })
  # oggetti utili per la funzione
  cond_dim <- (NCOL(x_tr) == 1)
  cond_shrink <- (NCOL(x_tr) < NROW(x_tr))
  n_ts <- ifelse(cond_dim, length(x_ts), NROW(x_ts))
  y_tr <- as.factor(y_tr)
  nomi_cl <- levels(y_tr)
  cond_dim <- (NCOL(x_tr) == 1)
  x_tr <- data.frame(map(x_tr,as.numeric))
  x_ts <- data.frame(map(x_ts,as.numeric))
  tvals <- rep(0, 2);  prev <- rep(0, n_ts)
  # ciclo sulle osservazioni dell'insieme di verifica
  for(i in 1:n_ts){
    if(cond_dim){ xtsi <- x_ts[i] }
    else{ xtsi <- x_ts[i,] }
    ddt <- data.frame(yy = y_tr, x_tr) 
    # primi due passi dell'algoritmo
    for(cl in 1:2){
      if(cond_dim){ 
        dati <- data.frame(yy = as.factor(c(cl, y_tr)), 
                           c(xtsi, x_tr)) }
      else{ 
        dati <- data.frame(yy = as.factor(c(cl, y_tr)), 
                           rbind(xtsi, x_tr)) }
      db1 <- dati[which(dati$yy == 1),-1]
      db2 <- dati[which(dati$yy == 2),-1]
      # t-test se input unidimensionale
      if(cond_dim){ tvals[cl] <- abs(t.test(db1,db2)$statistic) }
      # T2 di Hotelling se input multidimensionale
      else{  
        if(cond_shrink){ 
          tvals[cl] <- hotelling.stat(db1,db2)$statistic }
        # shrinkage se p >= n
        else{ tvals[cl] <- hotelling.stat(db1,db2, 
                                          shrinkage = TRUE)$statistic }}}
    # terzo passo dell'algoritmo 
    prev[i] <- nomi_cl[which.max(tvals)] } 
  
  return(prev) }
