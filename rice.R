setwd("C:/Users/vijay/Desktop/SRI trichoderma")

read.csv("SRI1.csv")->wheat
names(wheat)
attach(wheat)
as.factor(Rep)-> rep
as.factor(Var)-> Treat
as.factor(Mainplot)-> meth
as.factor(Subplot)->seed
res.bad <- lm(GY ~ Treat*meth*seed, data = wheat)
anova(res.bad)
library(agricolae)
model1<-aov(GY~rep+Treat)
model1
comp3<-LSD.test(model1,"Treat")
comp3

model2<-aov(GY~rep+meth)
comp4<-LSD.test(model2,"meth")
comp4

model3<-aov(GY~rep+seed)
comp5<-LSD.test(model3,"seed")
comp5

model6<-aov(GY~rep+Treat+meth+seed)
comp6<-LSD.test(model6, "Treat*meth")
comp6
