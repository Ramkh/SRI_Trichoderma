setwd("C:/Users/vijay/Desktop/SRI trichoderma")
SRI_chaite <- read_excel("C:/Users/vijay/Desktop/SRI trichoderma/SRI chaite.xlsx")
detach(SRI1)
attach(SRI_chaite)
SRI_chaite <- read_csv("submitted/PEERJ/SRI_chaite.csv")
attach(SRI_chaite)
names(SRI_chaite)
as.factor(Rep)
as.factor(`Main plot`)-> Seedtreat
as.factor(`Sub plot`)->Method
shapiro.test(SRI_chaite$`moisture%`)
shapiro.test(SRI_chaite$`height (cm)`)
shapiro.test(SRI_chaite$`tiller no`)
shapiro.test(SRI_chaite$`Days of maturity`)
shapiro.test(SRI_chaite$`Days of harvest`)
shapiro.test(SRI_chaite$`Yield (mt/ha)`)
shapiro.test(SRI_chaite$`Grain panicle`)
shapiro.test(SRI_chaite$`1000 grain weight (gm)`)



SRI1 <- read_csv("submitted/PEERJ/SRI1.csv")
detach(SRI_chaite)
attach(SRI1)
names(SRI1)
shapiro.test(SRI1$paniclelength)
shapiro.test(SRI1$filledgrain)
shapiro.test(SRI1$unfilledgrain)
shapiro.test(SRI1$tillerpersqm)
shapiro.test(SRI1$`1000 grain weight (gm)`)
shapiro.test(SRI1$`Grain Yield (mt/ha)`)

library(ggplot2)
library(ggExtra)
library(grid)
library(reshape2)
l <- ggplot(SRI_chaite, aes(Seedtreat, `Yield (mt/ha)`, fill= `Seedtreat`)) + geom_boxplot()+labs(x="", y="Grain Yield (mt/ha)")
e <-l + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point") 
m<-e+facet_wrap(c("Method"), ncol = 2)
k<-m +theme(legend.position="none")
k

s <- ggplot(SRI_chaite, aes(`Seed treat`, TGW,fill= `Seed treat`)) + geom_boxplot()+labs(x="", y="Test weight (gm)")
t <-s + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
u<-t+facet_wrap(c("Method"), ncol = 2)
v<-u+theme(legend.position="none")
v

a <- ggplot(SRI_chaite, aes(`Seed treat`, paniclelength,fill= `Seed treat`)) + geom_boxplot()+labs(x="Bio-innouclation", y="Panicle length (cm)")
b <-a + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
c<-b+facet_wrap(c("Method"), ncol = 2)
x<-c+theme(legend.position="none")
x
d <- ggplot(SRI_chaite, aes(`Seed treat`, tillerpersqm,fill= `Seed treat`)) + geom_boxplot()+labs(x="Bio-innoculation", y="Tiller per Sq.m")
f <-d + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
g<-f+facet_wrap(c("Method"), ncol = 2)
y<-g+theme(legend.position="none")
y

library(gridExtra)
grid.arrange(k, v,x,y, ncol=2)
