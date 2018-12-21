setwd("C:/Users/vijay/Desktop")
SRI <- read.csv("C:/Users/vijay/Desktop/SRI1.csv")
names(SRI)
factor("Rep")
factor("Var")
factor("Method")
factor("Seed.treat")
factor("Method*Var")
model <- lm(GY~ Var*Method*Seed.treat, data = SRI)
anova(model)
library(agricolae)
model1<-aov(GY ~Rep+Seed.treat, data=SRI)
model1
comp3<-LSD.test(model1,"Seed.treat")
comp3

model2<-aov(GY~Rep+Method, data=SRI)
comp4<-LSD.test(model2,"Method")
comp4

model3<-aov(GY~Rep+Var, data=SRI)
comp5<-LSD.test(model3,"Var")
comp5

SRI$Method.Var<-with(SRI,interaction(Method,Var))
model4<-aov(GY~Rep+Method.Var, data=SRI)
comp6<-LSD.test(model4,"Method.Var",alpha=0.001,console=TRUE)
comp6

SRI$Seed.treat.Var<-with(SRI,interaction(Seed.treat,Var))
model5<-aov(GY~Rep+Seed.treat.Var, data=SRI)
comp7<-LSD.test(model5,"Seed.treat.Var",alpha=0.001,console=TRUE)
comp7

SRI$Seed.treat.Var.Method<-with(SRI,interaction(Seed.treat,Var, Method))
model6<-aov(GY~Rep+Seed.treat.Var.Method, data=SRI)
comp8<-LSD.test(model6,"Seed.treat.Var.Method",alpha=0.001,console=TRUE)
comp8


par(mfrow=c(1,4))
op <-par(mar = c(5,3,0.5,0.01)+1); on.exit(par(op))
boxplot(GY~Seed.treat.Var.Method, data = SRI, col  = "skyblue", main= " Grain Yield (mt/ha)", 
        horizontal = F, axis=1, las=3, cex.axis = 0.9)
boxplot(TGW ~ Seed.treat.Var.Method, data = SRI, col  = "skyblue",  main = " Test Weight (gm) ", 
        horizontal = F, axis=2, las=2, cex.axis = 0.9)
boxplot( Panicle.length~ Seed.treat.Var.Method, data = SRI, col  = "skyblue",  main = " Panicle Lenght (cm) ", 
        horizontal = F, axis=2, las=2, cex.axis = 0.9)
boxplot(No.of.tiller.per.sq..m ~ Seed.treat.Var.Method, data = SRI, col  = "skyblue",  main = " Tiller per m^2", 
        horizontal = F, axis=2, las=2, cex.axis = 0.9)

model <- lm(Panicle.length~ Var*Method*Seed.treat, data = SRI)
anova(model)

model <- lm(No.of.filled.grains~ Var*Method*Seed.treat, data = SRI)
anova(model)

model <- lm(No.of.unfilled.grains~ Var*Method*Seed.treat, data = SRI)
anova(model)

model <- lm(No.of.tiller.per.sq..m~ Var*Method*Seed.treat, data = SRI)
anova(model)

model <- lm(TGW~ Var*Method*Seed.treat, data = SRI)
anova(model)
