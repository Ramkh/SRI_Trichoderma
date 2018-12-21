setwd("C:/Users/vijay/Desktop/SRI trichoderma")
SRI.chaite <- read.csv("C:/Users/vijay/Desktop/SRI trichoderma/SRI chaite.csv")
View(SRI.chaite)
attach(SRI.chaite)
names(SRI.chaite)
as.factor(Rep)
as.factor(Mainplot)->treat
as.factor(Subplot)->method
library(ggplot2)
library(Rmisc)
library(plyr)
library(ggExtra)
library(grid)
library(reshape2)
SRI.chaite1 <- summarySE(SRI.chaite, measurevar= ("height"), groupvars=c("treat", "method"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI.chaite1
o <- ggplot(SRI.chaite1, aes(treat, height, fill=  treat)) + geom_bar(stat="identity", color="black",  position=position_dodge())+labs(x=" ", y="Plant height (cm)")
p <-o + geom_errorbar(aes(ymin=height, ymax=height+sd), width=.2, position=position_dodge(.9)) +facet_grid(.~method) 
q<-p+theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
q

SRI.chaite2 <- summarySE(SRI.chaite, measurevar= ("Grain.panicle"), groupvars=c("treat", "method"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI.chaite2
r <- ggplot(SRI.chaite2, aes(treat, Grain.panicle, fill=  treat)) + geom_bar(stat="identity", color="black",  position=position_dodge())+labs(x=" ", y="Grain/panicle")
s <-r + geom_errorbar(aes(ymin=Grain.panicle, ymax= Grain.panicle +sd), width=.2, position=position_dodge(.9)) +facet_grid(.~method)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
s
t<-s+theme(legend.position="none")
t

SRI.chaite3 <- summarySE(SRI.chaite, measurevar= ("tiller.no"), groupvars=c("treat", "method"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI.chaite3
u <- ggplot(SRI.chaite3, aes(treat,tiller.no, fill=treat)) + geom_bar(stat="identity", color="black",  position=position_dodge())+labs(x=" ", y="Tiller per hill")
v <-u+geom_errorbar(aes(ymin=tiller.no, ymax= tiller.no+sd), width=.2, position=position_dodge(.9)) +facet_grid(.~method)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
w<-v+theme(legend.position="none")
w

SRI.chaite4 <- summarySE(SRI.chaite, measurevar= ("Yield..mt.ha."), groupvars=c("treat", "method"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI.chaite3
a <- ggplot(SRI.chaite4, aes(treat,Yield..mt.ha., fill=treat)) + geom_bar(stat="identity", color="black",  position=position_dodge())+labs(x=" ", y="Yeild (mt/ha)")
b <-a+geom_errorbar(aes(ymin=Yield..mt.ha., ymax= Yield..mt.ha.+sd), width=.2, position=position_dodge(.9)) +facet_grid(.~method)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
c<-b+theme(legend.position="none")
c

library(gridExtra)
grid.arrange(c,q, t, w, ncol=2)

