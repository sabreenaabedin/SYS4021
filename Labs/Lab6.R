# how many are spam - sum of those marked "1"
sum(spam$V58)

# LOG TRANSFORMED BOX PLOTS

# log transformed with an offset of 0.1 with V 1-9 
Lspam <- log(spam[,-58] +.1)
Lspam$V58 <- spam[,58]

# create boxplot comparisons 1-9
par(mfrow = c(3,3))
for(i in 1:9)
{
  boxplot(Lspam[,i]~Lspam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))  
}
par(mfrow = c(1,1))

# create boxplot comparisons 20-28
par(mfrow = c(3,3))
for(i in 20:28)
{
  boxplot(Lspam[,i]~Lspam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))  
}
par(mfrow = c(1,1))

# SCATTER PLOT MATRIX OF LOG TRANSFORMED VARIABLES
source("SPM_Panel.R")

par(mfrow = c(3,3))
for(i in 49:58)
{
  boxplot(log(spam[,i]+.01)~spam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))  
}
par(mfrow = c(1,1))

uva.pairs(Lspam[,c("V49","V50","V51","V52","V53","V54","V55","V56","V57","V58")])


# PRINCIPAL COMPONENTS FOR LOG TRANSFORMED VARIABLES

source("PCAplots.R")
source("FactorPlots.R")
Lspam.pc = princomp(Lspam[,1:57], cor = T)
biplot(Lspam.pc)

cumplot(Lspam.pc)

biplot.fact(Lspam.pc, Lspam[,58])
legend(-7, 10, legend = c("Spam", "Ham"), col = c("red", "blue"), pch = c(18, 19))

# MAIN EFFECT MODEL WITH FIRST AND LAST 10 VARIABLES (48-57)
sub <- Lspam[,c(1:10,48:58)]
spam.glm1 <- glm(V58~., data = sub, family = binomial)
spam.null <- glm(V58~1, data = spam, family = binomial)
anova(spam.null, spam.glm1, test = "Chi")

# DROP1
library(MASS)

drop1(spam.glm1, response~., test = "Chi", data = sub)

# STEPWISE
step <- step(spam.glm1, data = sub, family = binomial)

# PARTIAL LIKELIHOOD TEST

anova(spam.glm1, step, test = "Chi")

AIC(spam.glm1)
AIC(step)

# PREDICTIONS
spam.pred <- predict(spam.glm1, type = "response")
score.table(spam.pred, Lspam[,58], .5)

spam.pred.step <- predict(step, type = "response")
score.table(spam.pred.step, Lspam[,58], .5)

# ROC
plot.roc(spam.pred, Lspam[,58], main = "ROC Curve - SPAM Filter", col = "blue")
lines.roc(spam.pred.step, Lspam[,58], col = "orange")

# PCA
spampca.glm90 <- pc.glm(spam.pca, 98, spam[,58])
var.comp(spam.pca, 98)
summary(spampca.glm90)

spam.null <- glm(V58~1, data = spam, family = binomial)
anova(spam.null, spampca.glm90, test = "Chi")

score.table(spampca.glm90, Lspam[,58], .5)
