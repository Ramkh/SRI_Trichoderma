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
SRI.chaite1 <- summarySE(SRI.chaite, measurevar= ("height"), groupvars=c("treat", "method"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI.chaite1

library(ggExtra)
library(grid)
library(reshape2)
colnames(SRI.chaite1)=c("Mainplot", "Subplot",  "Netplot", "moisture.", "tiller.no", "height", "DOM", "DOH", "Adjusted.Yield", "Yield..mt.ha." )
o <- ggplot(SRI.chaite1, aes(treat, height, fill=  treat)) + geom_bar(stat="identity", color="black", 
                                                                       position=position_dodge())+labs(x=" ", y="Plant height (cm)", fill=ColorScheme)
p <-o + geom_errorbar(aes(ymin=height, ymax=height+sd), width=.2,
                      position=position_dodge(.9)) +facet_grid(.~method) 
q<-p+theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
q


SRI.chaite2 <- summarySE(SRI.chaite, measurevar= ("Grain.panicle"), groupvars=c("treat", "method"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI.chaite2
r <- ggplot(SRI.chaite2, aes(treat, Grain.panicle, fill=  treat)) + geom_bar(stat="identity", color="black",  position=position_dodge())+labs(x=" ", y="Grain/panicle")
s <-r + geom_errorbar(aes(ymin=Grain.panicle, ymax= Grain.panicle +sd), width=.2, position=position_dodge(.9)) +facet_grid(.~method)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
s


SRI.chaite3 <- summarySE(SRI.chaite, measurevar= ("tiller.no"), groupvars=c("treat", "Subplot"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI.chaite3
u <- ggplot(SRI.chaite3, aes(treat, tiller.no, fill=  treat)) + geom_bar(stat="identity", color="black",  position=position_dodge())+labs(x="Bio-innoculation ", y=" Tiller number per hill")
u
v <-u + geom_errorbar(aes(ymin = tiller.no, ymax = tiller.no +sd), width=.2, position=position_dodge(.9)) +facet_grid(.~Subplot) + theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
v


SRI.chaite4 <- summarySE(SRI.chaite, measurevar= ("Yield..mt.ha."), groupvars=c("treat", "method"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI.chaite4
k <- ggplot(SRI.chaite4, aes(treat,Yield..mt.ha., fill=  treat)) + geom_bar(stat="identity", color="black",  position=position_dodge())+labs(x=" ", y=" Yield (mt/ha)")
k
m <-k + geom_errorbar(aes(ymin= Yield..mt.ha., ymax= Yield..mt.ha.+sd), width=.2, position=position_dodge(.9)) +facet_grid(.~method)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
m


library(gridExtra)

grid.arrange(m,s, v, m, ncol=2)


s <-r + geom_errorbar(aes(ymin=Yield..mt.ha, ymax= Yield..mt.ha +sd), width=.2, position=position_dodge(.9)) +facet_grid(.~method)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
s




w<-v+theme(legend.position="none")
w

a <- ggplot(SRI.chaite, aes(Mainplot, tiller.no, fill=  Mainplot)) + geom_boxplot()+labs(x=" Bio-innoculation", y="Tiller number per hill")
b <-a + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+facet_grid(.~Subplot) 
c<-b+theme(legend.position="none")
c

library(gridExtra)
grid.arrange(q, t, w,c, ncol=2)








l <- ggplot(SRI1, aes(`Seed treat`, GY, fill= `Seed treat`)) + geom_boxplot()+labs(x="", y="Grain Yield (mt/ha)")
e <-l + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point") 
m<-e+facet_wrap(c("Var"), ncol = 2)
k<-m +theme(legend.position="none")
k

s <- ggplot(SRI1, aes(`Seed treat`, TGW,fill= `Seed treat`)) + geom_boxplot()+labs(x="", y="Test weight (gm)")
t <-s + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
u<-t+facet_wrap(c("Var"), ncol = 2)
v<-u+theme(legend.position="none")
v

a <- ggplot(SRI1, aes(`Seed treat`, paniclelength,fill= `Seed treat`)) + geom_boxplot()+labs(x="Bio-innouclation", y="Panicle length (cm)")
b <-a + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
c<-b+facet_wrap(c("Var"), ncol = 2)
x<-c+theme(legend.position="none")
x
d <- ggplot(SRI1, aes(`Seed treat`, tillerpersqm,fill= `Seed treat`)) + geom_boxplot()+labs(x="Bio-innoculation", y="Tiller per Sq.m")
f <-d + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
g<-f+facet_wrap(c("Var"), ncol = 2)
y<-g+theme(legend.position="none")
y

library(gridExtra)
grid.arrange(k, v,x,y, ncol=2)






h <- ggplot(SRI1, aes(`Seed treat`, "No of filled grains",fill= `Seed treat` )) + geom_boxplot()+labs(x="Bio-innoculation", y="Panicle length ()")
i <-h + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
j<-i+facet_wrap(c("Method"), ncol = 2)
j

library(gridExtra)
grid.arrange(m, u,c,d ncol=4)

n<m+facet_wrap(c("Var"), ncol = 4)
n<-m+theme(axis.text.x = element_text(size = 15, vjust=0,hjust = 1,  angle = 90),strip.text = element_text(size=12, lineheight=0.0001))
n+theme(axis.title.y=element_text(size=14))
