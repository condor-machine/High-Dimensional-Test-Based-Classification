source('C:/Users/zebra/Desktop/HDTBC article/codice e dati/codice_tot.R')
library(tidyverse)

nn <- c(10,12,14,16,18,20)
pp <- 100

accu_hdtbc <- matrix(0,nrow=100,ncol=6)
accu_sda <- matrix(0,nrow=100,ncol=6)


colnames(accu_hdtbc) <- c('n = 10', 'n = 12','n = 14', 'n = 16', 'n = 18', 'n = 20') -> colnames(accu_sda)

#
#
# # p fisso pari a 100
# # n che varia in (10,12,14,16,18,20)
# # metà variabili spiegano,
# # l'altra metà son rumore
# # per ogni valore di n calcolo
# # le performance 10 volte
#
k=0

for(i in 1:100){
  print(i)
  for(j in 1:6){

    set.seed(k*pi)
    k <- k + 1

    db <- twoClassSim(n=nn[j],linearVars=pp/2,noiseVars=pp/2)
    dbb <- data.frame(y=as.factor(db$Class), dplyr::select(db,-Class))
    while(min(table(dbb[,1]))<=4){
      set.seed(k*pi+k)
      k <- k + 1
      db <- twoClassSim(n=nn[j],linearVars=pp/2,noiseVars=pp/2)
      dbb <- data.frame(y=as.factor(db$Class), dplyr::select(db,-Class))
    }


    accu_hdtbc[i,j] <- perf_kfold(dbb,'tbc')$accu
    #print(accu_hdtbc[i,j])
    accu_sda[i,j] <- perf_kfold(dbb,'sda')$accu
    #print(accu_sda[i,j])

  }
}




accu_tot <- data.frame(rbind(accu_hdtbc,accu_sda),model = c(rep('hdtbc',100),rep('sda',100)))
names(accu_tot)[1:6] <- c('n = 10', 'n = 12', 'n = 14', 'n = 16', 'n = 18', 'n = 20')

#
accu_means <- accu_tot %>% group_by(model) %>% summarise_all(mean) %>%
  as.data.frame()
accu_se <- accu_tot %>% group_by(model) %>%
  summarise_all(function(x) sd(x)/sqrt(length(x))) %>%
  as.data.frame()


accu_dritta_n_bassa <- data.frame(model =  c(rep('hdtbc',6),rep('sda',6)),
                            accur = c(accu_means[1,-1][[1]],accu_means[1,-1][[2]],accu_means[1,-1][[3]],
                                      accu_means[1,-1][[4]],accu_means[1,-1][[5]],accu_means[1,-1][[6]],
                                      accu_means[2,-1][[1]],accu_means[2,-1][[2]],accu_means[2,-1][[3]],
                                      accu_means[2,-1][[4]],accu_means[2,-1][[5]],accu_means[2,-1][[6]]),
                            se = c(accu_se[1,-1][[1]],accu_se[1,-1][[2]],accu_se[1,-1][[3]],
                                   accu_se[1,-1][[4]],accu_se[1,-1][[5]],accu_se[1,-1][[6]],
                                   accu_se[2,-1][[1]],accu_se[2,-1][[2]],accu_se[2,-1][[3]],
                                   accu_se[2,-1][[4]],accu_se[2,-1][[5]],accu_se[2,-1][[6]]),
                            xxx = c(1:6,1:6))


save(accu_dritta_n_bassa,file="C:/Users/zebra/Desktop/HDTBC article/codice e dati/simulazioni/accu_n_bassa.Rda")
load('C:/Users/zebra/Desktop/HDTBC article/codice e dati/simulazioni/accu_n_bassa.Rda')


ggplot(accu_dritta_n_bassa, aes(x=xxx, y=accur, colour=model, group=model)) +
  geom_errorbar(aes(ymin=accur-se, ymax=accur+se), colour="black", width=.2, size=1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1),size=1) +
  geom_point(position=position_dodge(0.1), size=3, shape=21, fill="white") + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6),
                     labels=c("10", '12',"14", "16", '18', '20')) +
  xlab("Number of Observations") +
  ylab("Accuracy") +
  scale_colour_hue(name="Models:",    
                   breaks=c("hdtbc", "sda"),
                   labels=c("HDTBC", "SDA"),
                   l=50) +                   
  guides(fill=guide_legend(title="Models:")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0.03),
        legend.key = element_rect(colour = NA, fill = NA), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(colour = 'black', size = 16),
        axis.text.y = element_text(colour = 'black', size = 16),
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0), size = 17),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 17))






#############################################################################


source('C:/Users/zebra/Desktop/condor machine/codice_tot.R')
library(tidyverse)

nn <- c(10,20,30,40,50)
pp <- 100

accu_hdtbc <- matrix(0,nrow=100,ncol=5)
accu_sda <- matrix(0,nrow=100,ncol=5)


colnames(accu_hdtbc) <- c('n = 10','n = 20','n = 30', 'n = 40', 'n = 50') -> colnames(accu_sda) 



# p fisso pari a 100
# n che varia in (10,20,30,40,50)
# metà variabili spiegano,
# l'altra metà son rumore
# per ogni valore di n calcolo
# le performance 10 volte

k=0

for(i in 1:100){
  print(i)
  for(j in 1:5){
    
    set.seed(k*pi)
    k <- k + 1
    
    db <- twoClassSim(n=nn[j],linearVars=pp/2,noiseVars=pp/2)
    dbb <- data.frame(y=as.factor(db$Class), dplyr::select(db,-Class))
    while(min(table(dbb[,1]))<=4){
      set.seed(k*pi+k)
      k <- k + 1
      db <- twoClassSim(n=nn[j],linearVars=pp/2,noiseVars=pp/2)
      dbb <- data.frame(y=as.factor(db$Class), dplyr::select(db,-Class))
    }
    
    
    accu_hdtbc[i,j] <- perf_kfold(dbb,'tbc')$accu
    #print(accu_hdtbc[i,j])
    accu_sda[i,j] <- perf_kfold(dbb,'sda')$accu
    #print(accu_sda[i,j])
    
  }
}






accu_tot <- data.frame(rbind(accu_hdtbc,accu_sda),model = c(rep('hdtbc',100),rep('sda',100)))
names(accu_tot)[1:5] <- c('n = 10', 'n = 20', 'n = 30', 'n = 40', 'n = 50')


accu_means <- accu_tot %>% group_by(model) %>% summarise_all(mean) %>%
  as.data.frame()
accu_se <- accu_tot %>% group_by(model) %>% 
  summarise_all(function(x) sd(x)/sqrt(length(x))) %>%
  as.data.frame()


accu_dritta_n_alta <- data.frame(model =  c(rep('hdtbc',5),rep('sda',5)),
                            accur = c(accu_means[1,-1][[1]],accu_means[1,-1][[2]],accu_means[1,-1][[3]],
                                      accu_means[1,-1][[4]],accu_means[1,-1][[5]],
                                      accu_means[2,-1][[1]],accu_means[2,-1][[2]],accu_means[2,-1][[3]],
                                      accu_means[2,-1][[4]],accu_means[2,-1][[5]]),
                            se = c(accu_se[1,-1][[1]],accu_se[1,-1][[2]],accu_se[1,-1][[3]],
                                   accu_se[1,-1][[4]],accu_se[1,-1][[5]],
                                   accu_se[2,-1][[1]],accu_se[2,-1][[2]],accu_se[2,-1][[3]],
                                   accu_se[2,-1][[4]],accu_se[2,-1][[5]]),
                            xxx = c(1:5,1:5))




save(accu_dritta_n_alta,file="C:/Users/zebra/Desktop/condor machine/res_sim/accu_n_alta.Rda")
load('C:/Users/zebra/Desktop/HDTBC article/codice e dati/simulazioni/accu_n_alta.Rda')



ggplot(accu_dritta_n_alta, aes(x=xxx, y=accur, colour=model, group=model)) + 
  geom_errorbar(aes(ymin=accur-se, ymax=accur+se), colour="black", width=.2, size = 1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1),size=1) +
  geom_point(position=position_dodge(0.1), size=3, shape=21, fill="white") + # 21 is filled circle
  scale_x_continuous(breaks=c(1,2,3,4,5),
                     labels=c("10", "20", "30", '40', '50')) +
  xlab("Number of Observations") +
  ylab("Accuracy") +
  scale_colour_hue(name="Models:",    
                   breaks=c("hdtbc", "sda"),
                   labels=c("HDTBC", "SDA"),
                   l=50) +                   
  guides(fill=guide_legend(title="Models:")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0.03),
        legend.key = element_rect(fill = 'white'), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(colour = 'black', size = 16),
        axis.text.y = element_text(colour = 'black', size = 16),
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0), size = 17),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 17))





# pdf size when saving the plots: 4.5 x 9.5







