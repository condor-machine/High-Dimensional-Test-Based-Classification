res_rf <- c(.727,.971,.964,.918,.918,.898,.915,.525,
            .823,.832,.934,.93,.958,
            .826,.774,.984,.87,.814,.8,.805)

res_knn <- c(.636,.969,.964,.934,.755,.831,.789,.65,
             .774,.847,.789,.95,.917,
             .832,.742,.969,.817,.776,.1,.779)

res_en <- c(.727,.969,.964,.934,.878,.898,.901,.638,
            .823,.847,.961,.94,.958,
            .783,.769,.98,.755,.784,.7,.74)

res_svm <- c(.773,.971,.964,.934,.939,.932,.915,.588,
             .806,.847,.908,.95,.972,
             .79,.788,.98,.75,.78,.8,.779)

res_lda <- c(.772,.96,.964,.934,.919,.932,.8899,.625,
             .79,.84,.921,.96,.986,
             .792,.774,.934,.716,.789,.8,.753)

res_lda_knn <- c(NA,.957,NA,NA,NA,NA,NA,.6625,
                 NA,.84,NA,.96,NA,
                 .836,.771,NA,.803,.8,NA,.727)

res_tbc <- c(NA,.96,NA,NA,NA,NA,NA,.663,
             NA,.824,NA,.96,NA,
             .789,.769,.934,.707,.782,NA,.766)

res_ibc <- c(.636,.969,.964,.951,.755,.847,.831,.6125,
             .806,.832,.829,.94,.944,
             .83,.765,.969,.846,.796,.5,.766)

res_ibc_base <- c(.636,.946,.875,.77,.51,.746,.803,.475,
                  .806,.847,.5,.92,.958,
                  .707,.774,.93,.707,.78,.6,.701)

res_hd <- c(.864,.966,.964,.934,.919,.932,.915,.675,
            .823,.832,.947,.95,.986,.843,
            .765,.972,.861,.804,.8,.792)


n <- c(22,683,56,61,49,59,71,80,62,131,76,100,72,
       1000,532,256,208,714,10,77)


p <- c(300,10,600,600,600,600,600,5,200,10,
       931,4,500,2,7,6,60,7,20,6)


db_res <- data.frame(n=n,p=p,res_ibc=res_ibc,res_hd=res_hd,
                     res_rf = res_rf, res_en = res_en, 
                     res_knn = res_knn, res_svm = res_svm, 
                     res_lda = res_lda, res_tbc = res_tbc,res_ibc_base,res_lda_knn)

db_res <- db_res[order(db_res$p/db_res$n),]


dbres <- db_res[,-c(10,12)]
dbres[dbres$n>dbres$p,4] <- db_res[db_res$n>db_res$p,10]






####### tbc vs lda
res_ldatbc <- na.omit(dbres[,c(1,2,4,9)])
plot(x=1:20,y=res_ldatbc[,3],type='b',xlab='p/n',ylab='Accuratezza',xaxt='n',ylim=c(.4,1),lwd=2)
axis(1, at=1:20, labels = round(res_ldatbc$p/res_ldatbc$n,3))
lines(x=1:20,y=res_ldatbc[,4],col='red',type='b',lty=2,lwd=2)
abline(v=10.5,lty=3,lwd=3)
legend("bottomleft", c('TBC / HDTBC','LDA / SDA', 'p = n'),
       lty=c(1,2,3),col=c(1,2,1),lwd=c(2,2,3))


####### ibc vs ibc_knn vs knn vs tbc
res_ibc <- na.omit(dbres[,c(1,2,4,3)])
plot(x=1:20,y=res_ibc[,3],type='b',xlab='p/n',ylab='Accuratezza',xaxt='n',ylim=c(.4,1),lwd=2)
axis(1, at=1:20, labels = round(res_ibc$p/res_ibc$n,3))
lines(x=1:20,y=res_ibc[,4],col='red',type='b',lty=2,lwd=2)
abline(v=10.5,lty=3,lwd=3)
legend("bottomleft", c('TBC / HDTBC','IBC','p = n'), 
       lty=c(1,2,3),col=c(1,2,1),lwd=c(2,2,3))



####### tbc vs all
res_all <- na.omit(dbres[,c(1,2,4,5,6,8)])
plot(x=1:20,y=res_all[,3],type='b',xlab='p/n',ylab='Accuratezza',xaxt='n',ylim=c(.4,1),lwd=2)
axis(1, at=1:20, labels = round(res_all$p/res_all$n,3))
lines(x=1:20,y=res_all[,4],col='red',type='b',lty=2,lwd=2)
lines(x=1:20,y=res_all[,5],col='blue',type='b',lty=2,lwd=2)
lines(x=1:20,y=res_all[,6],col=7,type='b',lty=2,lwd=2)
abline(v=10.5,lty=3,lwd=3)
legend("bottomleft", c('TBC / HDTBC','RF','EN','SVM','p = n'), 
       lty=c(1,2,2,2,3),col=c(1,2,4,7,1),lwd=c(2,2,2,2,3))

