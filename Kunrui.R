#Test Train
setwd(codepath)
source("TestSet.R")


#Recode Derail with 1's and 0's 
dim(xactsndt25)
xactsndt25$Derail <- rep(0, nrow(xactsndt25))
xactsndt25$Derail[which(xactsndt25$Type == "Derailment")] <- 1
xactsndt25$Derail <- as.factor(xactsndt25$Derail)

#initialize pmse arrays for each model
pmse.result1 <- NULL
pmse.result2 <- NULL
pmse.result3 <- NULL
pmse.result4 <- NULL

#----------------------- Test & Train 
xactsndt25 <- xactsndt25[!(xactsndt25$Type == "Explosive"), ]
for (i in c(1:40)){
  #set test sets size: 
  test.size<-1/3
  # generate training sets and test sets from original data:
  xactsndt25.data <- test.set(xactsndt25, test.size) 
  
  
  # Build model with train set:
  lm1.acc.train <- lm(ACCDMG~TRNSPD+HIGHSPD+CARS+Derail+Cause+REGION, data = xactsndt25.data$train)
  lm2.acc.train <- lm(ACCDMG~TRNSPD+HIGHSPD+CARS+Derail+Cause, data = xactsndt25.data$train)
  lm3.acc.train <- lm(ACCDMG~TRNSPD+HIGHSPD+CARS+Derail+Cause+EVACUATE, data = xactsndt25.data$train)
  lm4.acc.train <- lm(ACCDMG~TRNSPD+HIGHSPD+CARS+Derail+Cause+EVACUATE+LOADF1+LOADP1, data = xactsndt25.data$train)
  
  # First, how to predict with lm models:
  lm1.acc.pred <- predict(lm1.acc.train, newdata = xactsndt25.data$test)
  lm2.acc.pred <- predict(lm2.acc.train, newdata = xactsndt25.data$test)
  lm3.acc.pred <- predict(lm3.acc.train, newdata = xactsndt25.data$test)
  lm4.acc.pred <- predict(lm4.acc.train, newdata = xactsndt25.data$test)
  
  # Next, compute PMSE:
  pmse.lm1 <- mse(lm1.acc.pred, xactsndt25.data$test$ACCDMG)
  pmse.lm2 <- mse(lm2.acc.pred, xactsndt25.data$test$ACCDMG)
  pmse.lm3 <- mse(lm3.acc.pred, xactsndt25.data$test$ACCDMG)
  pmse.lm4 <- mse(lm4.acc.pred, xactsndt25.data$test$ACCDMG)
  
  # Add the PMSE for this run into your vector to stor PMSE
  pmse.result1 <- c(pmse.result1, pmse.lm1)
  pmse.result2 <- c(pmse.result2, pmse.lm2)
  pmse.result3 <- c(pmse.result3, pmse.lm3)
  pmse.result4 <- c(pmse.result4, pmse.lm4)
}

#Check arrays
#pmse.result1
#pmse.result2
#pmse.result3
#pmse.result4

#plot & compare PMSE
plot(pmse.result1, type = 'b', col = 'blue', xlab = "Index", ylab = "PMSE")
lines(pmse.result2, type = 'b', col = 'red')
lines(pmse.result3, type = 'b', col = 'green')
lines(pmse.result4, type = 'b', col = 'orange')
title(main = "Model Comparison Based on PMSE")

#proceeding with model 4
lm4.acc <- lm(ACCDMG~TRNSPD+HIGHSPD+CARS+Derail+Cause+EVACUATE+LOADF1+LOADP1, data = xactsndt25)
lm4.acc.step <- step(lm4.acc, direction = "both", trace = T)
summary(lm4.acc.step)
#diagnostic plots
par(mfrow=c(2,2))
plot(lm4.acc.step)
par(mfrow=c(1,1))
#not normal; boxcox transformation needed
#boxcox - detemrining lambda
library(MASS)
boxcox(lm4.acc)
title(main = "Box-Cox Transformation Plot for Model 4")
L <- boxcox(lm4.acc, plotit=F)$x[which.max(boxcox(lm4.acc, plotit=F)$y)]
#boxcox - transformation 
lm4.acc.boxcox <- lm(ACCDMG^L~ TRNSPD + HIGHSPD + CARS + Derail + Cause + EVACUATE + LOADF1 + LOADP1, data = xactsndt25)
summary(lm4.acc.boxcox)
#negative AIC? 
AIC(lm4.acc.boxcox)
#boxcox - diagnostics
par(mfrow = c(2,2))
plot(lm4.acc.boxcox)
par(mfrow = c(1,1))

#checking interactions
lm4.acc.interactions <- lm(ACCDMG~(TRNSPD+HIGHSPD+CARS+Derail+Cause+EVACUATE+LOADF1+LOADP1)^2, data = xactsndt25)
lm4.acc.interactions.step <- step(lm4.acc.interactions, direction = "both", trace = T)
summary(lm4.acc.interactions.step)

xyplot(log(ACCDMG)~TRNSPD | Type, data = xactsndt25, type = c("p", "r"), main = "Interaction plot Type and Train Speed on log(Accident Damage)")
xyplot(log(Casualty)~TRNSPD | Type, data = xcasndt14, type = c("p", "r"), main = "Interaction plot Type and Train Speed on log(Casualty)")

source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xactsndt25$Cause, xactsndt25$Type), title = "# Extreme Accidents by Cause and Type of Accident")
myImagePlot(table(xcasndt14$Cause, xcasndt14$Type), title = "# Extreme Casualties by Cause and Type of Accident")

xactsndt25$Derail <- (xactsndt25$Type == "Derailment")
interaction.plot(xactsndt25$Derail, xactsndt25$Cause, xactsndt25$ACCDMG, xlab = "Derailement", ylab="Accident Damage $", main = "Interaction between Derailment and Cause")

xcasndt14$HwyRail <- (xcasndt14$Type == "Hwy-Rail")
interaction.plot(xcasndt14$HwyRail, xcasndt14$Cause, xcasndt14$Casualty, xlab = "Hwy-Rail Accident", ylab="# Casualties", main = "Interaction between Hwy-Rail and Cause")
xcasndt14$HwyRail
