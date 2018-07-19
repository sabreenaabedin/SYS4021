x#************************************************************
#				SPAM Filter 1 
#			Graphical Analysis of SPAM 
#************************************************************

#*****************************************
# Load Data
#*****************************************
setwd("/Users/sabreenaabedin/Desktop/class/SYS4021") 
spam <- read.table("Spam.txt", sep = " ", header = F)

source("SPM_Panel.R")
source("PCAplots.R")
source("FactorPlots.R")

#***************************
#	Graphical Analysis
#***************************

dim(spam)
summary(spam)

# response variable
table(spam$V58)

# What proportion is spam?
sum(spam[,58])/length(spam[,58])

uva.pairs(spam[,c(1:10,58)])
uva.pairs(log(spam[,c(1:10,58)]+.00001))


# Obtain boxplots with variables 1-9 vs. the response.
par(mfrow = c(3,3))
for(i in 1:9)
{
  boxplot(spam[,i]~spam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))  
}
par(mfrow = c(1,1))

# Obtain boxplots with variables 49-57 vs. the response.
par(mfrow = c(3,3))
for(i in 49:57)
{
  boxplot(log(spam[,i]+.01)~spam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))  
}
par(mfrow = c(1,1))


#**************************
# Principal Components
#**************************

# Obtain the principal components for variables 1-57. 
spam.pca = princomp(spam[,1:57], cor = T)
biplot(spam.pca)

# What is the outlier?
summary(spam[,which(spam.pca$loadings[,2] > 0.2)])
spam[1754,which(spam.pca$loadings[,2] > 0.2)]
boxplot(spam$V56)

# Obtain the biplot.fact of your principal components.
biplot.fact(spam.pca, spam[,58])
legend(20, 15, legend = c("Spam", "Ham"), pch = c(18, 19), col = c("red", "blue"))

#************************
#		Log of Predictors
#************************

# log transform of the predictor variables
Lspam <- log(spam[,-58] + .1)
Lspam[,58] <- spam[,58]

# Obtain box plots for log transforms of variables 1-9 with variable 58.
par(mfrow = c(3,3))
sapply(1:9, function(i,M,z){boxplot(M[,i]~z, main = paste("Variable ", i), xlab = "spam")}, z = Lspam[,58], M = Lspam)
par(mfrow = c(1,1))


# Obtain the principal components for the log transform of variables 1-57. 
Lspam.pc = princomp(Lspam[,1:57], cor = T)
biplot(Lspam.pc)

# Obtain the biplot.fact of your principal components for the log transformed variables.

biplot.fact(Lspam.pc, Lspam[,58])
legend(-7, 10, legend = c("Spam", "Ham"), col = c("red", "blue"), pch = c(18, 19))




#************************************************************
#				SPAM Filter 2 
#			Generalized Lnear Models of SPAM 
#************************************************************

#*******
# GLM
#*******

# Use the glm() function to obtain the glm for the spam filter.
spam.glm.main <- glm(V58~., data = spam, family = binomial)
summary(spam.glm.main)

# Create a model with just the capital letters, V55 - V58  as predictors
spam.cap <- glm(V58~., data = spam[,55:58], family = binomial)

spam[1,]
predict(spam.glm.main, newdata = spam[1,])
exp(predict(spam.glm.main, newdata = spam[1,]))
exp(predict(spam.glm.main, newdata = data.frame(spam[1,c(1:50, 52:57)], V51 = 1)))


#************************************************************
#				            SPAM Filter  
#			Part 1:  Generalized Linear Models of SPAM 
#     Part 2:  GLM Principal Components Regression
#     Part 3:  Evaluation of Generalized Linear Models
#************************************************************

#***********************************************************
#	Part 1:  Generalized Linear Models of SPAM GLM
#***********************************************************

# Use the glm() function to obtain the glm for the spam filter.
spam.glm.main <- glm(V58~., data = spam, family = binomial)


# Model Utility Test using Chi2 statistic
spam.null <- glm(V58~1, data = spam, family = binomial)
anova(spam.null, spam.glm.main, test = "Chi")

# Create a model with just the capital letters, 
spam.cap <- glm(V58~., data = spam[,55:58], family = binomial)

# run model utility test
anova(spam.null, spam.cap, test = "Chi")

# Use the likelihood ratio or partial chi square test to compare the full main effects model with the capital letters model
anova(spam.cap, spam.glm.main, test = "Chi")

# Use t-test to test the coefficient on V57
spam.no57 <- update(spam.glm.main, .~.-V57, data = spam)
anova(spam.no57, spam.glm.main, test = "Chi")

# What is the contribution of variable 51 to the model?
# notice that we add one to the variable number (51) to account for the intercept

(exp(spam.glm.main$coefficients[52])-1)*100

# Explain this result.
spam[1,]
predict(spam.glm.main, newdata = spam[1,])
exp(predict(spam.glm.main, newdata = spam[1,]))
exp(predict(spam.glm.main, newdata = data.frame(spam[1,c(1:50, 52:57)], V51 = 1)))

# sequentially drops terms and compares to complete model
library(MASS)
drop1(spam.glm.main, response~., test = "Chi", data = dspam)

#  Compare the step model with capital letter predictors to the capital letter model
step.cap <- step(spam.cap, data = spam, family = binomial)

#*****************************
# GLM with Interactions
#*****************************

spam.glm <- glm(V58~., data = spam, family = binomial)
spam.glm2 <- glm(V58~. + (V5+V6+V7)^2, data = spam, family = binomial)
anova(spam.glm, spam.glm2, test = "Chi")

# Now compare a main effects model with log transformed predictors 
Lspam <- log(spam[,-58] +.1)
Lspam$V58 <- spam[,58]

Lspam.glm <- glm(V58~., data = Lspam, family = binomial)
# interaction terms
Lspam.glm2 <- glm(V58~. + (V5+V6+V7)^2, data = Lspam, family = binomial)

#partial likelihood test
anova(Lspam.glm, Lspam.glm2, test = "Chi")


#********************************
#	Evaluate performance with AIC
#********************************
AIC(spam.glm)
AIC(spam.glm2)
AIC(Lspam.glm)
AIC(Lspam.glm2)

# Compare the BIC for the 4 models you developed.
BIC(spam.glm)
BIC(spam.glm2)
BIC(Lspam.glm)
BIC(Lspam.glm2)

#***********************************************************
#  Part 2:  GLM Principal Components Regression
#***********************************************************

# obtain the principal components for the predictors with a correlation matrix
spam.pca <- princomp(spam[,-58], cor = T)

# Scree plot
screeplot(spam.pca)

# to see how many components are needed for 90% of the variance we use
var.comp(spam.pca, 90)

# Use pc.glm() to get the principal component regression results.
spampca.glm98 <- pc.glm(spam.pca, 98, spam[,58])

# Do a model utility test starting with pc.null()
spampc.null <- pc.null(spam.pca, 98, spam[,58])
anova(spampc.null, spampca.glm98, test = "Chi")


# Do a model utility test for model that uses PC that account for 90% of the variance
spampca.glm90 <- pc.glm(spam.pca, 90, spam[,58])
anova(spampc.null, spampca.glm90, test = "Chi")

# Do a model utility test for model that uses PC that account for 50% of the variance
spampca.glm50 <- pc.glm(spam.pca, 50, spam[,58])
anova(spampc.null, spampca.glm50, test = "Chi")

# Do a partial likelihood test between the 98 % and 50% models
anova(spampca.glm50, spampca.glm98, test = "Chi")

AIC(spampca.glm98)
AIC(spampca.glm90)
AIC(spampca.glm50)

BIC(spampca.glm98)
BIC(spampca.glm90)
BIC(spampca.glm50)


#***********************************************************
#	Part 3:  Evaluation of Generalized Linear Models
#***********************************************************

#************************
# Test and training sets
#************************

# Obtain test and training sets from the spam data
Spam <- test.set(spam, .33)
barplot(matrix(c(sum(spam$V58)/length(spam$V58), (length(spam$V58)-sum(spam$V58))/length(spam$V58), sum(Spam$train$V58)/length(Spam$train$V58), (length(Spam$train$V58)-sum(Spam$train$V58))/length(Spam$train$V58),sum(Spam$test$V58)/length(Spam$test$V58), (length(Spam$test$V58)-sum(Spam$test$V58))/length(Spam$test$V58) ), nrow = 2, byrow = F, dimnames = list(NULL, c("Full", "Train", "Test"))), beside = F, col = c("mistyrose", "lightblue"))

# Predictor Variables comparisons of data sets
par(mfrow = c(2,2))
for(i in sample(1:58,4))
{
  boxplot(spam[,i], at = 1, xaxt = "n", xlim = c(0, 4), main = paste("V",i))
  boxplot(Spam$test[,i], at = 2, xaxt = "n", add = TRUE)
  boxplot(Spam$train[,i], at = 3, xaxt = "n", add = TRUE)
  axis(1, at = 1:3, labels = c("Original", "Test", "Train"), tick = TRUE)
}

#*****************************
# GLM with training data
#*****************************

# Obtain a GLM with all predictor variables using test data
spam.glm <- glm(V58~., data = Spam$train, family = binomial)
resid(spam.glm)
summary(spam.glm)

# Test the model utility
spam.null <- glm(V58~1, data = Spam$train, family = binomial)
anova(spam.null, spam.glm, test = "Chi")

# Obtain a GLM with the log transform of all predictor variables using the training data
LSpam.train <- log(Spam$train[,-58] + .1)
LSpam.train$V58 <- Spam$train$V58
Lspam.glm <- glm(V58~., LSpam.train, family = binomial)
summary(Lspam.glm)

# Test the model utility
Lspam.null <- glm(V58~1, data = LSpam.train, family = binomial)
anova(Lspam.null, Lspam.glm, test = "Chi")

#*****************************
# Diagnostic plots
#*****************************

par(mfrow = c(2,2))
plot(spam.glm)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(Lspam.glm)
par(mfrow = c(1,1))

plot(spam.glm, which=4)

# Influential points

which(hatvalues(spam.glm) > 0.8)
which(hatvalues(Lspam.glm) > 0.8)

Spam$train[which(hatvalues(spam.glm) > 0.8),]
summary(Spam$train)


#*****************************************
#	Variable Selection with Stepwise
#****************************************

# main effects stepwise (do at home)
spam.step <- step(spam.glm, trace = 0) 

# How many variables?
length(spam.step$coeff) - 1

# Do stepwise with 5 steps
spam.step1 <- step(spam.glm, steps = 5, trace = 0) 

# How many variables in step1? 
length(spam.step1$coeff) - 1

# stepwise for the log transform
Lspam.step <- step(Lspam.glm)

# For class do it with 5 steps
Lspam.step1 <- step(Lspam.glm, steps = 5, trace = 0) 

#***********************
# Interactions
#***********************

V1.factor<-spam$V1
V1.factor[which(spam$V1<=median(spam$V1))]<-'low'
V1.factor[which(spam$V1>median(spam$V1))]<-'high'

V38.factor<-spam$V38
V38.factor[which(spam$V38<=median(spam$V38))]<-'low'
V38.factor[which(spam$V38>median(spam$V38))]<-'high'

with(spam, interaction.plot(x.factor=V38.factor, trace.factor=V1.factor, response=V58, type="b",ylab="Percent Spam", main="Interaction Plot",pch=c(1,19)))

#*********************************
#	Principal Component Regression
#*********************************

# obtain a GLM with principal components accounting for 90% of the variance using the training data.
spam.pca <- princomp(Spam$train[,-58], cor = T)
spampca.glm90 <- pc.glm(spam.pca, 90, Spam$train[,58])

# accounting for 98% of the variance.
spam.pca <- princomp(Spam$train[,-58], cor = T)
spampca.glm98 <- pc.glm(spam.pca, 98, Spam$train[,58])


#*****************************
# Predictions with test data
#*****************************

# Use the test data to obtain predictions for the GLM with all predictor variables
spam.pred <- predict(spam.glm, type = "response", newdata = Spam$test)

# Use the test data to obtain predictions for the  
# GLM with the log transform of all predictor variables

Lspam.test <- log(Spam$test +.1)
Lspam.pred <- predict(Lspam.glm, type = "response", newdata = Lspam.test)

# Use the test data GLM with principal components accounting for 98% of the variance.
spampca.pred <- predict.pc.glm(spampca.glm98, spam.pca,Spam$test[,1:57] )

# Obtain predictions from the stepwise models
step.pred1 <- predict(spam.step1,  type = "response", newdata = Spam$test)
Lstep.pred1 <- predict(Lspam.step1,  type = "response", newdata = Lspam.test)

#*****************************
#  Confusion Matrices
#*****************************

# Set the decision threshold to 0.5, compute Confusion Matrices for each model
score.table(spam.pred, Spam$test[,58], .5)
score.table(Lspam.pred, Spam$test[,58], .5)
score.table(spampca.pred, Spam$test[,58], .5)
score.table(step.pred1, Spam$test[,58], .5)
score.table(Lstep.pred1, Spam$test[,58], .5)

#***************
#   ROC Curves
#***************

plot.roc(spam.pred, Spam$test[,58], main = "ROC Curve - SPAM Filter", col = "blue")
lines.roc(Lspam.pred, Spam$test[,58], col = "orange")
lines.roc(spampca.pred, Spam$test[,58], col = "green")
lines.roc(step.pred1, Spam$test[,58], col = "red")
lines.roc(Lstep.pred1, Spam$test[,58], col = "cyan")
legend(.6, .57, legend = c("Main Effects", "Log", "PCR.", "Step", "Logstep"), col = c("blue", "orange", "green", "red", "cyan"), lwd = 1)


#************************************************************
#
#      	SPAM Filter  
#			Time Series Analysis 3
#
#
#************************************************************

#*****************************
#
# Load the data & source files
#
#*****************************

##load the 'ham_ts.csv' and 'spam_ts.csv' into ham and spam variables repectively
ham<-read.table('ham_ts.csv',header=T,sep=',')
spam<-read.table('spam_ts.csv',header=T,sep=',')

##use the ts() command to get a time series of ham amount using ham data from all years with the exception of the last 6 weeks.
ham.ts<-ts(ham$count[1:464])

##use the ts() command to get a time series of spam amount using spam data from all years, but disclude the final 2 weeks (this will be your test set)
spam.ts<-ts(spam$count)

##load the forecast library; first run command 'install.packages("forecast")'
library(forecast)



#*****************************
# 
# Spam  LM Models
# Trend and Seasonality  
# 
#*****************************
# Create a new variable time.spam which contains the index of the days 
# minus the last 7 which we will use as a test set


time.spam<-c(1:(length(spam.ts)-7))

# Get a linear model of spam vs. the index of the day

spam.trend<-lm(spam.ts[time.spam]~time.spam)

# Use the summary() command on spam.trend,
# Is time significant in predicting spam frequency?

summary(spam.trend)


plot(spam.ts[time.spam], type = "l")
abline(spam.trend, col = "red")


#*****************************
# 
# AR, MA & ARIMA Models  
# 
#***************************** 

#get the residuals from the spam.trend model above and store in e.ts:

e.ts.spam<-ts(spam.trend$residuals)

##plot the residuals for the spam.trend model

plot(e.ts.spam, ylab = "Residuals from Trend Model")



# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals spam.trend
par(mfrow=c(1,2))
acf(e.ts.spam, main="ACF of Residuals from spam.trend")
pacf(e.ts.spam,main="PACF of Residuals from spam.trend")
par(mfrow=c(1,1))

# Does this time series still have structure to be modeled?

##Yes


# Do we need to consider a first order difference of our residuals?
par(mfrow=c(1,2))
acf(diff(e.ts.spam), main="Diff ACF of Residuals from spam.trend")
pacf(diff(e.ts.spam),main="Diff PACF of Residuals from spam.trend")
par(mfrow=c(1,1))


#*****************************
# 
# AR, MA & ARIMA Models  
# 
#*****************************

# Let us choose p and q terms for e.ts.spam based on the acf and pacf 

# arma(1,1) p=1, q=1
spam.arma11 <- arima(e.ts.spam, order=c(1,0,1))
summary(spam.arma11)
AIC(spam.arma11)

# ar(1) p=1
spam.ar1 <- arima(e.ts.spam, order=c(1,0,0))
summary(spam.ar1)
AIC(spam.ar1)

# arima(1,1,1) p=1, d=1, q=1
spam.arima111 <- arima(e.ts.spam, order=c(1,1,1))
summary(spam.arima111)
AIC(spam.arima111)

# Use the auto.arima from 'forecast' library- Automatically find p, q, & d terms
spam.auto <- auto.arima(e.ts.spam, trace=TRUE)
summary(spam.auto)
AIC(spam.auto)

# Based on AIC, which one do you choose? What if we use BIC and diagnostics?

# BIC
AIC(spam.auto, k = log(length(e.ts.spam)))



# Plot diagnostics for models model using tsdiag()

tsdiag(spam.arma11,gof.lag=20)

tsdiag(spam.ar1,gof.lag=20)

tsdiag(spam.arima111,gof.lag=20)

tsdiag(spam.auto,gof.lag=20)


# Which diagnostics plots are adequate?  Which are not?


# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals spam.auto
par(mfrow=c(1,2))
acf(spam.auto$residuals, main="ACF of Residuals from spam.auto")
pacf(spam.auto$residuals,main="PACF of Residuals from spam.auto")
par(mfrow=c(1,1))

# Does the spam.auto model account for correlation in the residuals?

# Yes

# Forecasting:

# Use the forecast option to forecast the next 7 days of spam data- spam.auto.forecast
spam.auto.forecast<-forecast(spam.auto,h=7)

plot(spam.auto.forecast)

# Prediction performance
# Create test set from spam data set with last 7 days

# the next week or the test period in days

next.week.time<-c((length(spam.ts)-6):(length(spam.ts)))

# the test data frame

next.week<-data.frame(time.spam = next.week.time, count = spam.ts[next.week.time])

# the actual time series for the test period

next.week.ts <- spam.ts[next.week.time]

next.week.ts<-ts(next.week$count)

# Prediction for the next week by spam.auto:

E_Y.pred<-predict(spam.trend,newdata=next.week)
e_t.pred<-forecast(spam.auto,h=7)
next.week.prediction<-E_Y.pred+e_t.pred$mean

# MSE:

mean((next.week.prediction-next.week$count)^2)

# Plot actual values and predicted values

plot(ts(next.week$count),type='o')
lines(ts(next.week.prediction),col='red',type='o')
lines(1:7, E_Y.pred + e_t.pred$lower[,2], col = "red", lty = "dashed")
lines(1:7, E_Y.pred + e_t.pred$upper[,2], col = "red", lty = "dashed")
legend(4,12, legend = c("Actual", "Predicted"), lwd = 2, col = c("black", "red")) 


#**********************
# Another model
# Spam with just ARIMA
#**********************

#Plot acf and pacf of spam.ts[time.spam]


# Find the best model for the original data


# How does it do? Diagnostics?


# Forecasting:


# Prediction performance

# Prediction for the next week:

# MSE:

# Plot actual values and predicted values

