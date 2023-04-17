library(jtools)
library(ggplot2)
library(interactions)
install.packages("interactions")
install.packages( "stargazer")
library(stargazer)

data <- read.table('treemoist.dat')
colnames(data) <- c('Species', 'Branches', 'BranchLocations', 'TranspirationTypes', 'MoistureContent')
dim(data)
str(data)
summary(data)

fit1 <- lm(MoistureContent ~ Species + Branches + BranchLocations + TranspirationTypes, data=data)
summary(fit1)

fit2 <- step(lm(MoistureContent ~ Species + Branches + BranchLocations + TranspirationTypes, data=data), direction='both')

aov1 <- aov(MoistureContent ~ Species*TranspirationTypes, data=data)
summary(aov1)

install.packages("xtable")
library(xtable)
xtable(summary(aov1))


# plot 1
data2 <- data
data2$TranspirationTypes <- factor(data2$TranspirationTypes)
data2$Species <- factor(data2$Species)
fit5 <- lm(MoistureContent ~ Species*TranspirationTypes, data=data)
summary(fit5)
fit3 <- lm(MoistureContent ~ Species*TranspirationTypes, data=data2)
summary(fit3)
cat_plot(fit3, pred=Species, modx=TranspirationTypes)

##
fit4 <- lm(MoistureContent ~ Species * BranchLocations * TranspirationTypes, data=data)
summary(fit4)

aov2 <- aov(MoistureContent ~ Species * BranchLocations * TranspirationTypes, data=data) # 结果不一样
summary(aov2)
xtable(summary(aov2))


aov3 <- aov(MoistureContent ~ BranchLocations * TranspirationTypes, data=data)
summary(aov3)
xtable(summary(aov3))

aov4 <- aov(MoistureContent ~ BranchLocations * Species, data=data)
summary(aov4)


# plot 2
data3 <- data
data3$Species <- factor(data3$Species, labels=c('Loblolly Pine', 'Shortleaf Pine', 'Yellow Poplar', 'Red Gum'))
data3$BranchLocations <- factor(data3$BranchLocations)
data3$TranspirationTypes <- factor(data$TranspirationTypes)
fit6 <- lm(MoistureContent ~ Species * BranchLocations * TranspirationTypes, data=data3)
summary(fit6)
cat_plot(fit6, pred=BranchLocations, modx=TranspirationTypes, mod2=Species)

## Model assessment
best <- lm(MoistureContent ~ BranchLocations + TranspirationTypes + Species:TranspirationTypes , data = data)
par(mfrow=c(1,2)) 
plot(best,which = 2)
plot(best, which = 1)
grid(col="black") 

bartlett.test(resid(best) ~ interaction(data$BranchLocations,data$TranspirationTypes))


