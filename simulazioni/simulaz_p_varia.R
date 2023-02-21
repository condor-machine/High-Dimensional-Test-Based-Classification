source('C:/Users/zebra/Desktop/HDTBC article/codice e dati/codice_tot.R')
library(tidyverse)

nn <- 10
pp <- c(10,100,1000)

accu_hdtbc <- matrix(0,nrow=100,ncol=3)
accu_sda <- matrix(0,nrow=100,ncol=3)


colnames(accu_hdtbc) <- c('p = 10','p = 100','p = 1000') -> colnames(accu_sda) 



# n fisso pari a 10
# p che varia in (10,100,1000)
# metà variabili spiegano,
# l'altra metà son rumore.
# per ogni valore di p calcolo
# le performance 100 volte

k=0

for(i in 1:100){
  
  print(i)
  
  for(j in 1:3){
    
    set.seed(k*pi)
    k <- k + 1
    
    db <- twoClassSim(n=nn,linearVars=pp[j]/2,noiseVars=pp[j]/2)
    dbb <- data.frame(y=as.factor(db$Class), dplyr::select(db,-Class))
    while(min(table(dbb[,1]))<4){
      set.seed(k*pi+k)
      k <- k + 1
      db <- twoClassSim(n=nn,linearVars=pp[j]/2,noiseVars=pp[j]/2)
      dbb <- data.frame(y=as.factor(db$Class), dplyr::select(db,-Class))
    }
    
    
    accu_hdtbc[i,j] <- perf_kfold(dbb,'tbc')$accu
    #print(accu_hdtbc[i,j])
    accu_sda[i,j] <- perf_kfold(dbb,'sda')$accu
    #print(accu_sda[i,j])
    
  }
}



accu_tot <- data.frame(rbind(accu_hdtbc,accu_sda),model = c(rep('hdtbc',100),rep('sda',100)))
names(accu_tot)[1:3] <- c('p = 10', 'p = 100', 'p = 1000')


accu_means <- accu_tot %>% group_by(model) %>% summarise_all(mean) %>%
  as.data.frame()
accu_se <- accu_tot %>% group_by(model) %>% 
  summarise_all(function(x) sd(x)/sqrt(length(x))) %>%
  as.data.frame()


accu_dritta_p <- data.frame(model =  c(rep('hdtbc',3),rep('sda',3)),
                          accur = c(accu_means[1,-1][[1]],accu_means[1,-1][[2]],accu_means[1,-1][[3]],
                                    accu_means[2,-1][[1]],accu_means[2,-1][[2]],accu_means[2,-1][[3]]),
                          se = c(accu_se[1,-1][[1]],accu_se[1,-1][[2]],accu_se[1,-1][[3]],
                                 accu_se[2,-1][[1]],accu_se[2,-1][[2]],accu_se[2,-1][[3]]),
                          xxx = c(1:3,1:3))




save(accu_dritta_p,file="C:/Users/zebra/Desktop/HDTBC article/codice e dati/simulazioni/accu_p_n10.Rda")
load('C:/Users/zebra/Desktop/HDTBC article/codice e dati/simulazioni/accu_p_n10.Rda')


ggplot(accu_dritta_p, aes(x=xxx, y=accur, colour=model, group=model)) + 
  geom_errorbar(aes(ymin=accur-se, ymax=accur+se), colour="black", width=.2, size=1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1),size=1) +
  geom_point(position=position_dodge(0.1), size=3, shape=21, fill="white") +      
  scale_x_continuous(breaks=c(1,2,3),
                   labels=c("10", "100", "1000")) +
  xlab(expression(paste('Numbers of predictors on ', log[10], ' scale'))) +
  ylab("Accuracy") +
  scale_colour_hue(name="Models:",
                   breaks=c("hdtbc", "sda"),
                   labels=c("HDTBC", "SDA"),
                   l=50) +
  guides(fill=guide_legend(title="Models:")) +
  theme(legend.justification=c(1,0),
        legend.position=c(0.16,0.03),
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



#################################################################
#################################################################
#################################################################


source('C:/Users/zebra/Desktop/condor machine/codice_tot.R')
library(tidyverse)

nn <- 20
pp <- c(20,100,1000)

accu_hdtbc <- matrix(0,nrow=100,ncol=3)
accu_sda <- matrix(0,nrow=100,ncol=3)


colnames(accu_hdtbc) <- c('p = 20','p = 100','p = 1000') -> colnames(accu_sda) 



# n fisso pari a 20
# p che varia in (10,100,1000)
# metà variabili spiegano,
# l'altra metà son rumore.
# per ogni valore di p calcolo
# le performance 100 volte

k=0

for(i in 1:100){
  
  print(i)
  
  for(j in 1:3){
    
    set.seed(k*pi)
    k <- k + 1
    
    db <- twoClassSim(n=nn,linearVars=pp[j]/2,noiseVars=pp[j]/2)
    dbb <- data.frame(y=as.factor(db$Class), dplyr::select(db,-Class))
    while(min(table(dbb[,1]))<=4){
      set.seed(k*pi+k)
      k <- k + 1
      db <- twoClassSim(n=nn,linearVars=pp[j]/2,noiseVars=pp[j]/2)
      dbb <- data.frame(y=as.factor(db$Class), dplyr::select(db,-Class))
    }
    
    
    accu_hdtbc[i,j] <- perf_kfold(dbb,'tbc')$accu
    #print(accu_hdtbc[i,j])
    accu_sda[i,j] <- perf_kfold(dbb,'sda')$accu
    #print(accu_sda[i,j])
    
  }
}



accu_tot <- data.frame(rbind(accu_hdtbc,accu_sda),model = c(rep('hdtbc',100),rep('sda',100)))
names(accu_tot)[1:3] <- c('p = 20', 'p = 100', 'p = 1000')


accu_means <- accu_tot %>% group_by(model) %>% summarise_all(mean) %>%
  as.data.frame()
accu_se <- accu_tot %>% group_by(model) %>% 
  summarise_all(function(x) sd(x)/sqrt(length(x))) %>%
  as.data.frame()


accu_dritta_p <- data.frame(model =  c(rep('hdtbc',3),rep('sda',3)),
                            accur = c(accu_means[1,-1][[1]],accu_means[1,-1][[2]],accu_means[1,-1][[3]],
                                      accu_means[2,-1][[1]],accu_means[2,-1][[2]],accu_means[2,-1][[3]]),
                            se = c(accu_se[1,-1][[1]],accu_se[1,-1][[2]],accu_se[1,-1][[3]],
                                   accu_se[2,-1][[1]],accu_se[2,-1][[2]],accu_se[2,-1][[3]]),
                            xxx = c(1:3,1:3))




save(accu_dritta_p,file="C:/Users/zebra/Desktop/HDTBC article/codice e dati/simulazioni/accu_p_n20.Rda")
load('C:/Users/zebra/Desktop/HDTBC article/codice e dati/simulazioni/accu_p_n20.Rda')


ggplot(accu_dritta_p, aes(x=xxx, y=accur, colour=model, group=model)) + 
  geom_errorbar(aes(ymin=accur-se, ymax=accur+se), colour="black", width=.2, size=1, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1),size=1) +
  geom_point(position=position_dodge(0.1), size=3, shape=21, fill="white") + # 21 is filled circle         
  scale_x_continuous(breaks=c(1,2,3),
                     labels=c("10", "100", "1000")) +
  
  xlab(expression(paste('Numbers of predictors on ', log[10], ' scale'))) +
  ylab("Accuracy") +
  scale_colour_hue(name="Models:",    # Legend label, use darker colors
                   breaks=c("hdtbc", "sda"),
                   labels=c("HDTBC", "SDA"),
                   l=50) +                    
  guides(fill=guide_legend(title="Models:")) +
  theme(legend.justification=c(1,0),
        legend.position=c(0.16,0.03),
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



# pdf size when saving the plot: 4.5 x 9.5




