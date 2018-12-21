SRI1 <- read_csv("submitted/PEERJ/SRI1.csv")
read.csv("SRI1.csv")->biomass
names(biomass)
attach(biomass)
as.factor(Rep)-> rep
as.factor(Method)-> Mainplot
as.factor(Seed.treat)-> Subplot
as.factor(Var)->var
model <- lm(paniclelength ~ Mainplot * Subplot*var, data = biomass)
anova(model)
library(agricolae)
model1<-aov(paniclelength~rep+Mainplot)
model1
LSD.test(model1, "Mainplot",console=TRUE)
model2<-aov(paniclelength~rep+Subplot)
comp4<-LSD.test(model2,"Subplot")
comp4
model3<-aov(paniclelength~rep+var)
comp5<-LSD.test(model3,"var")
comp5
model9 <- lm(paniclelength ~ Subplot*var, data = biomass)
anova(model9)

biomass$Subplot.var<-with(biomass,interaction(Subplot.var))
modle8<-aov(paniclelength~Subplot.var,data=biomass)
comp6<-LSD.test(modle8,"Subplot.var",alpha=0.001,console=TRUE) 
comp6
biomass$Subplot.Mainplot<-with(biomass,interaction(Subplot,Mainplot))
modle10<-aov(paniclelength~Subplot.Mainplot,data=biomass)
comp10<-LSD.test(modle10,"Subplot.Mainplot",alpha=0.001,console=TRUE) 
comp10

biomass$Subplot.var.Mainplot<-with(biomass,interaction(Subplot,var,Mainplot))
modle10<-aov(paniclelength~Subplot.var.Mainplot,data=biomass)
comp10<-LSD.test(modle10,"Subplot.var.Mainplot",alpha=0.001,console=TRUE) 
comp10

