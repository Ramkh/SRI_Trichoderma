SRI1 <- read_csv("C:/Users/vijay/Desktop/SRI trichoderma/SRI1.csv")
attach(SRI1)
names(SRI1)
as.factor(Rep)
as.factor(Var)
as.factor(Method)
as.factor(`Seed treat`)
library(ggplot2)
library(ggExtra)
library(grid)
library(reshape2)
l <- ggplot(SRI1, aes(`Seed treat`, GY, fill= `Seed treat`)) + geom_boxplot()+labs(x="", y="Grain Yield (mt/ha)")
e <-l + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point") 
m<-e+facet_wrap(c("Method"), ncol = 2)
k<-m +theme(legend.position="none")
k

s <- ggplot(SRI1, aes(`Seed treat`, TGW,fill= `Seed treat`)) + geom_boxplot()+labs(x="", y="Test weight (gm)")
t <-s + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
u<-t+facet_wrap(c("Method"), ncol = 2)
v<-u+theme(legend.position="none")
v

a <- ggplot(SRI1, aes(`Seed treat`, paniclelength,fill= `Seed treat`)) + geom_boxplot()+labs(x="Bio-innouclation", y="Panicle length (cm)")
b <-a + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
c<-b+facet_wrap(c("Method"), ncol = 2)
x<-c+theme(legend.position="none")
x
d <- ggplot(SRI1, aes(`Seed treat`, tillerpersqm,fill= `Seed treat`)) + geom_boxplot()+labs(x="Bio-innoculation", y="Tiller per Sq.m")
f <-d + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")
g<-f+facet_wrap(c("Method"), ncol = 2)
y<-g+theme(legend.position="none")
y

library(gridExtra)
grid.arrange(k, v,x,y, ncol=2)


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
