SRI1 <- read_csv("SRI1.csv")
detach(SRI1)
attach(SRI1)
names(SRI1)
as.factor(Rep)
as.factor(Var)->var
as.factor(Method)->method
as.factor(`Seed treat`)->bio

library(ggplot2)
library(Rmisc)
library(plyr)
SRI11 <- summarySE(SRI1, measurevar= ("GY"), groupvars=c("var", "method", "bio"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI11

library(ggExtra)
library(grid)
library(reshape2)
colnames(SRI11)=c("Mainplot", "Subplot",  "Netplot", "moisture.", "tiller.no", "height", "DOM", "DOH", "Adjusted.Yield", "Yield..mt.ha." )
o <- ggplot(SRI11, aes(var, GY, fill=  var)) + geom_bar(stat="identity", color="black", 
                                                                      position=position_dodge())+labs(x=" ", y="Grain Yield (mt/ha)")
p <-o + geom_errorbar(aes(ymin=GY, ymax=GY+sd), width=.2,
                      position=position_dodge(.9)) +facet_grid(.~bio) 
q<-p+theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
q

SRI112 <- summarySE(SRI1, measurevar= ("paniclelength"), groupvars=c("var", "method", "bio"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI112


r <- ggplot(SRI112, aes(var, paniclelength, fill= var)) + geom_bar(stat="identity", color="black", 
                                                        position=position_dodge())+labs(x=" ", y="Panicle Lenght (cm)")
r
s <-r +geom_errorbar(aes(ymin=paniclelength, ymax= paniclelength+sd), width=.2, position=position_dodge(.9)) +facet_grid(.~bio)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
s


SRI13 <- summarySE(SRI1, measurevar= ("tillerpersqm"), groupvars=c("var", "method", "bio"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI13
u <- ggplot(SRI13 , aes(var, tillerpersqm, fill= var)) + geom_bar(stat="identity", color="black", 
                                                                   position=position_dodge())+labs(x=" ", y=" Tiller Per sq. m (cm)")
u
v <-u + geom_errorbar(aes(ymin = tillerpersqm, ymax = tillerpersqm +sd), width=.2, position=position_dodge(.9)) +facet_grid(.~bio) + theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
v


SRI14 <- summarySE(SRI1, measurevar= ("TGW"), groupvars=c("var", "method", "bio"),na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
SRI14
k <- ggplot(SRI14 , aes(var, TGW, fill= var)) + geom_bar(stat="identity", color="black", 
                                                                  position=position_dodge())+labs(x=" ", y=" Test Weight (gm)")
k
m <-k + geom_errorbar(aes(ymin= TGW, ymax= TGW+sd), width=.2, position=position_dodge(.9)) +facet_grid(.~method)+ theme(legend.position="none")+scale_fill_manual(values=c("#CCCCCC","#FFFFFF"))
m


library(gridExtra)

grid.arrange(q, s, v, m, ncol=2)
