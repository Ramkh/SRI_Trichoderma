setwd("C:/Users/vijay/Desktop/SRI trichoderma")
biomass <- read.csv("SRI_chaite.csv")
names(biomass)
View(biomass)
as.factor(Rep)-> Rep
as.factor(Mainplot)-> Mainplot
as.factor(Subplot)-> Subplot

detach(biomass)
names(biomass)  
attach(biomass)
library(agricolae)
model <- lm(X1000.grain.weight..gm. ~ Mainplot * Subplot, data = biomass)
anova(model)
model1<-aov(X1000.grain.weight..gm.~Rep + Mainplot, data= biomass)
model1
LSD.test(model1, "Mainplot",console=TRUE)
model2<-aov (X1000.grain.weight..gm.~Rep+Subplot, data= biomass)
comp4<-LSD.test(model2,"Subplot")
comp4

model9 <- lm (X1000.grain.weight..gm. ~ Subplot*Mainplot, data = biomass)
anova(model9)


biomass$Subplot.Mainplot<-with(biomass,interaction(Subplot,Mainplot))
modle10<-aov(X1000.grain.weight..gm.~Subplot.Mainplot,data=biomass)
comp10<-LSD.test(modle10,"Subplot.Mainplot",alpha=0.001,console=TRUE) 
comp10

library(DescTools)
PT = NemenyiTest(x= X1000.grain.weight..gm., Subplot, data=biomass, dist="tukey")
PT
library(rcompanion)
cldList(comparison = PT$Comparison,
        p.value = PT$P.adj,
        threshold = 0.05)

library(ggplot2)
library(Rmisc)
library(plyr)
SRI113 <- summarySE(biomass, measurevar= ("Grain.panicle"), groupvars=c("Mainplot", "Subplot"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI113

library(ggExtra)
library(grid)
library(reshape2)
o <- ggplot(SRI113, aes(Mainplot, Grain.panicle, fill=Mainplot)) + geom_bar(stat="identity", color="black", 
                                                        position=position_dodge())+labs(x=" ", y="Grain per Panicle")
p <-o + geom_errorbar(aes(ymin=Grain.panicle, ymax=Grain.panicle+sd), width=.2,
                      position=position_dodge(.9)) +facet_grid(.~Subplot) 
q<-p+theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#000000"))
q

SRI114 <- summarySE(biomass, measurevar= ("Yield..mt.ha."), groupvars=c("Mainplot", "Subplot"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI114

a <- ggplot(SRI114, aes(Mainplot, Yield..mt.ha., fill=Mainplot)) + geom_bar(stat="identity", color="black", 
                                                                            position=position_dodge())+labs(x=" ", y="Yield (mt/ha)")
b <-a + geom_errorbar(aes(ymin=Yield..mt.ha., ymax=Yield..mt.ha.+sd), width=.2,
                      position=position_dodge(.9)) +facet_grid(.~Subplot) 
c<-b+theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#000000"))
c


SRI115 <- summarySE(biomass, measurevar= ("height..cm."), groupvars=c("Mainplot", "Subplot"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI115

d <- ggplot(SRI115, aes(Mainplot, height..cm., fill=Mainplot)) + geom_bar(stat="identity", color="black", 
                                                                            position=position_dodge())+labs(x=" ", y="Plant Height (cm)")
e <-d + geom_errorbar(aes(ymin=height..cm., ymax=height..cm.+sd), width=.2,
                      position=position_dodge(.9)) +facet_grid(.~Subplot) 
f<-e+theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#000000"))
f

SRI116 <- summarySE(biomass, measurevar= ("tiller.no"), groupvars=c("Mainplot", "Subplot"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI116

g <- ggplot(SRI116, aes(Mainplot, tiller.no, fill=Mainplot)) + geom_bar(stat="identity", color="black", 
                                                                          position=position_dodge())+labs(x=" ", y="Tiller per Hill")
h <-g + geom_errorbar(aes(ymin=tiller.no, ymax=tiller.no+sd), width=.2,
                      position=position_dodge(.9)) +facet_grid(.~Subplot) 
i<-h+theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#000000"))
i

library(gridExtra)

grid.arrange(c, f, q, i, ncol=2)


