mba <- read_csv("PCA2.csv")
detach(MFA)
attach(mba)


install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library(ggplot2)
colnames(mba)
res.mfa <- MFA(mba, group = c(1, 1, 1, 1, 1, 1),type = c("n", "s", "s", "s", "s", "s"),  name.group = c("treatments","yield","panicle", "grain", "test", "tiller" ),
               num.group.sup = c(1, 6),  graph = FALSE)
library("factoextra")
ind <- get_mfa_ind(res.mfa)
ind
fviz_mfa_ind(res.mfa, col.ind = "cos2", shape.ind =  19, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)



res.mfa <- MFA(PCA2, 
               group = c(1, 2, 2, 1), 
                             name.group = c("treatments","yield","panicle", "grain" ),
               num.group.sup = c(1, 7),
               graph = FALSE)

