setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")
Spam<-read.table('Spam.txt',header=F)
source("SPM_Panel.R")
source("PCAplots.R")
source("FactorPlots.R")
source("pc.glm.R")
source("ROC.R")
source("TestSet.R")
library(MASS)

factor(Spam$V58)

# without log
spam.pc = princomp(spam[,1:57], cor = T)
biplot(spam.pc)
cumplot(spam.pc)
loadingsplot(spam.pc)

# with log
Lspam <- log(spam[,-58] +.1)
Lspam$V58 <- spam[,58]
Lspam.pc = princomp(spam[,1:57], cor = T)
biplot(Lspam.pc)
cumplot(Lspam.pc)
loadingsplot(Lspam.pc)

# graphical analysis
par(mfrow = c(4,4))
for(i in 1:57)
{
  boxplot(log(spam[,i]+.01)~spam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))  
}
par(mfrow = c(1,1))


# t-test equivalent - tells us individual factor
drop1(spam.main, response~., test = "Chi", data = Lspam)

# MODEL CREATION

# main
spam.main <- glm(V58~., data = Lspam, family = binomial)

# likelihood test
spam.null <- glm(V58~1, data = Lspam, family = binomial)
anova(spam.null, spam.main, test = "Chi")

# stepwise model
spam.step <- step(spam.main, data = Lspam, family = binomial)
summary(spam.step)

# partial likelihood test
anova(spam.main, spam.step, test = "Chi")

# pca model
spam.pca <- pc.glm(Lspam.pc, 95, Lspam[,58])

# boxplot based model
spam.spec <- Lspam.glm.4 <- glm(V58~ V3 + V5 + V12 + V16 + V21 + V52 + V53, data = Lspam, family = binomial)

anova(spam.main, spam.spec, test = "Chi")



# STEPWISE ANALYSIS
AIC(spam.step) # AIC
AIC(spam.step,k=log(nrow(Lspam))) #BIC
summary(spam.step)$adj.r.squared
summary(spam.step)

par(mfrow = c(2,2))
plot(spam.step, cex.caption = 1.5, cex.id = 1, cex.lab = 1.5)
mtext("Diagnostic Plots for the Stepwise Model", side = 3, line = -2, outer = TRUE, cex = 1.5)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(spam.step)
par(mfrow = c(1,1))
plot(spam.step,labels.id = NULL, which=4) 


falsepos.array1 <- NULL
falsepos.array2 <- NULL
falsepos.array3 <- NULL
falsepos.array4 <- NULL
falseneg.array1 <- NULL
falseneg.array2 <- NULL
falseneg.array3 <- NULL
falseneg.array4 <- NULL

# false negatives and positives
for(i in 1:1)
{
  Lspam <- test.set(Lspam, .33)
  spam.main.train <- glm(V58~., data = Lspam$train, family = binomial)
  spam.step.train <- glm(formula = V58 ~ V1 + V3 + V5 + V6 + V7 + V8 + V12 + V13 + 
                           V14 + V15 + V16 + V17 + V18 + V20 + V21 + V23 + V24 + V25 + 
                           V27 + V28 + V29 + V33 + V35 + V36 + V37 + V39 + V41 + V42 + 
                           V43 + V44 + V45 + V46 + V48 + V49 + V50 + V52 + V53 + V55 + 
                           V57, family = binomial, data = Lspam)
  spam.pca.train <- pc.glm(spam.pc, 95, Lspam$train[,58])
  spam.spec.train <- glm(V58~ V3 + V5 + V12 + V16 + V21 + V52 + V53, data = Lspam$train, family = binomial)
 
  spam.main.predict <- predict(spam.main.train, newdata = Lspam$test)
  spam.step.predict <- predict(spam.step.train, newdata = Lspam$test)
  spam.pca.predict <- predict(spam.pca.train, newdata = Lspam$test)
  spam.spec.predict <- predict(spam.spec.train, newdata = Lspam$test)
  
  main.score <- score.table(spam.main.pred, Lspam$test[,58], .5)
  step.score <- score.table(spam.step.pred, Lspam$test[,58], .5)
  pca.score <- score.table(spam.pca.pred, Lspam$test[,58], .5)
  spec.score <- score.table(spam.spec.pred, Lspam$test[,58], .5)
  
  falsepos.array1 <- c(falsepos.array1, main.score[[2]])
  falseneg.array1 <- c(falsepos.array1, main.score[[3]])
  
  falsepos.array2 <- c(falsepos.array2, step.score[[2]])
  falseneg.array2 <- c(falsepos.array2, step.score[[3]])
  
  falsepos.array3 <- c(falsepos.array3, pca.score[[2]])
  falseneg.array3 <- c(falsepos.array3, pca.score[[3]])
  
  falsepos.array4 <- c(falsepos.array4, spec.score[[2]])
  falseneg.array4 <- c(falsepos.array4, spec.score[[3]])
}

spam <- test.set(Spam, .33)
spam.pred <- predict(spam.step, type = "response", newdata = spam$test)
score.table(spam.pred, spam$test[,58], .5)

plot.roc(spam.pred, Lspam[,58], main = "ROC Curve - SPAM Filter", col = "blue")
lines.roc(spam.pred.step, Lspam[,58], col = "orange")

plot(spam.main,labels.id = NULL, which=4) 


# pca model
#using just spam (no transform) 
spam.pca <- princomp(spam[, 1:57], cor = T)
spam.pca95 <- pc.glm(spam.pca, 95, spam[, 58])
spam.pca.null <- pc.null(spam.pca, 95, spam[, 58])
anova(spam.pca95, spam.pca.null, test = "Chi") #Chi used because it's a GLM 
summary(spam.pca95) #48 components used, only some of which are significant 
#using Lspam (with transform) 
Lspam.pca <- princomp(Lspam[, 1:57], cor = T)
Lspam.pca95 <- pc.glm(Lspam.pca, 95, Lspam[, 58])
Lspam.pca.null <- pc.null(Lspam.pca, 95, Lspam[, 58])
anova(Lspam.pca95, Lspam.pca.null, test = "Chi") #Chi used because it's a GLM
summary(Lspam.pca95) #46 components used, only some of which are significant
#Diagnostic Plots
par(mfrow = c(2,2))
plot(Lspam.pca95, cex.caption = 1.5, cex.id = 1, cex.lab = 1.5)
mtext("Diagnostic Plots for the PCA-95% Model", side = 3, line = -2, outer = TRUE, cex = 1.5)
par(mfrow = c(1,1))
#Cook's Distance
plot(Lspam.pca95,labels.id = NULL, which=4, cex.caption = 1.5, cex.lab = 1.5) #Cook's distance


#AIC, BIC, PMSE, False Negatives, False Positives
AIC(Lspam.pca95)
AIC(Lspam.pca95,k=log(nrow(Lspam))) #BIC
