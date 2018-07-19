#***************************************************************
#
#      Project 3: Simulation & Bootstrapping
#   		 A Case Study: Transplant Center
#   		   
#***************************************************************

#***************************************************************
#
#  Read in the data
#
#***************************************************************

#Set working directory
setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")

#Read data
r11xplant <- read.table("R11xplant.csv", sep = ",", header = T)
r11donor<-read.table("R11donor.csv", sep = ",", header = T)
uva <- read.table("UVAxplant.csv", sep = ",", header = T)
unc <- read.table("UNCxplant.csv", sep = ",", header = T)
mcv <- read.table("MCVxplant.csv", sep = ",", header = T)
duke <- read.table("Dukexplant.csv", sep = ",", header = T)

library(forecast)
#    Source the bootstrapping functions
library(boot) #If you don't have this library, install it by: install.packages('boot')
source("TSbootfunctions.R")
source("SPM_Panel.R")

#***************************************************************
#
# Part 1: Basic Statistics
#
#***************************************************************

#Step 1.1 Compare the performance of UVa with MCV kidney transplants

#Get the distribution of uva$Kidney, mcv$Kidney, r11donor$Kidney. What do you observe? 
plot(density(mcv$Kidney))
plot(density(uva$Kidney))
plot(density(r11donor$Kidney))
  # all distributions have bimodal plots, uva and r11 have more pronounced differences

#On average, how many kidney transplants are performed at UVa per year? MCV?
mean(uva$Kidney) # 68.533 ~ 69
mean(mcv$Kidney) # 85.7 ~ 86


#Perform a paired t-test between uva and mcv kidney transplants
uva.kidney<-uva$Kidney
mcv.kidney<-mcv$Kidney
t.test(uva.kidney, mcv.kidney,paired=T)
  #What is the result? p-value is low, uva and mcv have significantly different numbers of transplants


#Step 1.2 Compare the performance of UVa with Duke kidney transplants 

#Get the distribution of uva$Kidney, Duke$Kidney, r11xplant$Kidney. What do you observe?
plot(density(duke$Kidney))
plot(density(uva$Kidney))
plot(density(r11xplant$Kidney))
  # uva and r11 are still bimodal, duke may be bimodal but is smoothed over


#On average, how many kidney transplants are performed at UVa per year? Duke?
mean(uva$Kidney) # 68.533 ~ 69
mean(duke$Kidney) # 89.166 ~ 89


#Perform a paired t-test between uva and duke kidney transplants
uva.kidney<-uva$Kidney
duke.kidney<-duke$Kidney
t.test(uva.kidney, duke.kidney,paired=T) 
  #What is the result? low p-value, uva and duke are significantly different


#Step 1.3 Use bootstrapping to test the hypothesis: there is not a significant difference between UVa and MCV kidney transplants.

#What are the standard errors of the mean? What're the 95% confidence intervals? Do you accept or reject the null hypothesis?

# Compute the difference between uva kidney transplants and mcv kidney transplants from 1988 to 2016
kid.diff<-ts(uva.kidney-mcv.kidney,1988,2016)
ts.plot(kid.diff,ylab='UVa-MCV',main = "Difference in Number of Transplants, UVA-MCV")

bs.mean<-function(x,i) { return(mean(x[i])) }

# Bootstrap mean differences - syntax: boot(data= , statistic= , R= ), R = # replications
bs.kid.diff<-boot(kid.diff,bs.mean,R=2000)
bs.kid.diff # standard error = 5.521304

plot(bs.kid.diff,index=1) 

# confidence intervals using bca and percentile
boot.ci(bs.kid.diff,0.95,type=c('bca','perc'))
# 95%   (-28.31,  -6.28 )   (-28.70,  -6.66 )  

# We reject the null hypothesis that there is no difference between the two centers
# Our 95% confidence interval does not include 0, which would've implied no difference. 
# There is a significant difference in kidney transplants


#Step 1.4 Use bootstrapping to test the hypothesis: There is not a significant difference between UVa and Duke kidney transplants.

#What are the standard errors of the mean? What're the 95% confidence intervals? Do you accept or reject the null hypothesis?

# Compute the difference between uva kidney transplants and mcv kidney transplants from 1988 to 2016
kid.diff2<-ts(uva.kidney-duke.kidney,1988,2016)
ts.plot(kid.diff2,ylab='UVa-Duke',main = "Difference in Number of Transplants, UVA-Duke")

bs.mean<-function(x,i) { return(mean(x[i])) }

# Bootstrap mean differences - syntax: boot(data= , statistic= , R= ), R = # replications
bs.kid.diff2<-boot(kid.diff2,bs.mean,R=2000)
bs.kid.diff2 # standard error = 4.184158

plot(bs.kid.diff2,index=1) 

# confidence intervals using bca and percentile
boot.ci(bs.kid.diff2,0.95,type=c('bca','perc'))
# 95%   (-29.28, -12.93 )   (-28.86, -12.72 )

# We reject the null hypothesis that there is no difference between the two centers
# Our 95% confidence interval does not include 0, which would've implied no difference. 
# There is a significant difference in kidney transplants



#Step 1.5 Get the scatter plot matrix with the above 4 variables (UVA kidney, Duke kidney, R11 trnasplants, R11 donors). Describe what you observe. You can use either uva.pairs() {source("SPM_Panel.R")} or pairs().
pairs(~ uva$Kidney[-30] + duke$Kidney[-30] + r11donor$Kidney[-30] + r11xplant$Kidney[-30])
  # there are high linear correlations between every variable, especially between r11 transplants and r11 donors


#***************************************************************
#
# Part 2: Linear Regression Models
#
#***************************************************************

# Test the hypothesis: There is no difference between the forecasted numbers of kidney 
# transplants that will performed at UVA and at MCV in 2017.

# Step 2.1 Build a linear model: 
# uva$Kidney-mcv$Kidney = b0+b1*r11donor$Kidney+e. Call it uva.kidney.lm.
# Analyze the result: R^2, model utility test, t-tests, etc.

uva.kidney.lm<-lm(kid.diff~r11donor$Kidney[-30])
summary(uva.kidney.lm) # is significant

# Model Utility Test using Chi2 statistic
kidney.null <- lm(kid.diff~1, data = r11donor)
anova(kidney.null, uva.kidney.lm, test = "Chi")
  # model utility test is significantly different with a p-value of <= 0.001
  # the linear model is more useful than the null (proportional) model

#Step 2.2 Generate the diagnostic plots. Interpret the results of the diagnostic plots. Do you see any problem?

par(mfrow=c(2,2))
plot(uva.kidney.lm)
par(mfrow=c(1,1))
# non- constant variance, non-normal distribution, high cook's distnace 


# Step 2.3 Estimate the model with bootstrapping (by residuals). Is b1 significant?
# Get the fitted values from the regression model
uva.kfit <- fitted(uva.kidney.lm)

#    Get the residuals from the regression model
uva.ke <- residuals(uva.kidney.lm)

#    Get the regression model
uva.mod <- model.matrix(uva.kidney.lm)

#   Bootstrapping LM
uva.kidney.boot <- RTSB(uva$Kidney[-30], r11donor$Kidney[-30], uva.kfit, uva.ke, uva.mod,5000)
# outcome variable, input variable, fitted, residuals, parameters, # replications
uva.kidney.boot$t
sqrt(abs(var(uva.kidney.boot$t)))


#    95% CI of r11donor
boot.ci(uva.kidney.boot, .95, index=2)

#    Distribution of b1
par(mfrow = c(1,2))
hist(uva.kidney.boot$t[,2], main = "Region 11 Donors",xlab ="Coefficient Values",   col = "steelblue", breaks = 50)
qqnorm(uva.kidney.boot $t[,2])
qqline(uva.kidney.boot $t[,2])
par(mfrow = c(1,1))
  # much better fit to the empirical distribution

#    Is b1 significant?
#  our 95% confidence interval is (-0.1020, -0.0424 ), which does not include 0
# we can reject the null hypothesis that b1 = 0, which would've made b1 insignificant
# r11 donors is significant because b1 is significant


#Step 2.4* (bonus) What about Duke? Repeat the above steps and compare the results. 
# Test the hypothesis: There is no difference between the forecasted numbers of kidney 
# transplants that will be performed at UVA and at Duke in 2017.

## Step 2.1 (Duke) Build a linear model: 
# uva$Kidney-duke$Kidney = b0+b1*r11donor$Kidney+e. Call it uvaduke.kidney.lm.
# Analyze the result: R^2, model utility test, t-tests, etc.


uvaduke.kidney.lm<-lm(kid.diff2~r11donor$Kidney[-30])
summary(uvaduke.kidney.lm) # not significant at 0.05 significance level (p=0.1382), low adj R^2 (0.04548)

# Model Utility Test using Chi2 statistic
kidney.null.duke <- lm(kid.diff2~1, data = r11donor)
anova(kidney.null.duke, uvaduke.kidney.lm, test = "Chi")
# model utility test is not significantly different with a p-value of 0.12
# not more useful than the proportional model

#Step 2.2 (Duke) Generate the diagnostic plots. Interpret the results of the diagnostic plots. Do you see any problem?

par(mfrow=c(2,2))
plot(uvaduke.kidney.lm)
par(mfrow=c(1,1))
# non- constant variance, non-normal distribution, high cook's distnace 

# Step 2.3 (Duke) Estimate the model with bootstrapping (by residuals). Is b1 significant?
# Get the fitted values from the regression model
uvaduke.kfit <- fitted(uvaduke.kidney.lm)

#    Get the residuals from the regression model
uvaduke.ke <- residuals(uvaduke.kidney.lm)

#    Get the regression model
uvaduke.mod <- model.matrix(uvaduke.kidney.lm)

#   Bootstrapping LM
uvaduke.kidney.boot <- RTSB(uva$Kidney[-30], r11donor$Kidney[-30], uvaduke.kfit, uvaduke.ke, uvaduke.mod,5000)
# outcome variable, input variable, fitted, residuals, parameters, # replications
uvaduke.kidney.boot$t
sqrt(abs(var(uvaduke.kidney.boot$t)))


#    95% CI of r11donor
boot.ci(uvaduke.kidney.boot, .95, index=2)
#(-0.0522,  0.0042) zero is included in interval so uva and duke not significantly different


#    Distribution of b1
par(mfrow = c(1,2))
hist(uvaduke.kidney.boot$t[,2], main = "Region 11 Donors",xlab ="Coefficient Values",   col = "steelblue", breaks = 50)
qqnorm(uvaduke.kidney.boot $t[,2])
qqline(uvaduke.kidney.boot $t[,2])
par(mfrow = c(1,1))
#end duke





#***************************************************************
#
# Part 3: Time Series Models
#
#***************************************************************
#Step 3.1 Generate the ACF and PACF plots of the residuals from your part 2 linear model for UVA and MCV kidney transplants. Interpret the results of the ACF and PACF plots. Do you recommend modeling the residuals? If so, what kind of model should you try based on these plots?  
par(mfrow = c(1,2))
acf(uva.kidney.lm$residuals)
pacf(uva.kidney.lm$residuals)
par(mfrow = c(1,1))
  # lags are significant, we should use an AR model with a lag of 2

#Step 3.2 Based on the above ACF and PACF plots, fit an ar model to the residuals

#    Add the AR model of the residuals to regression linear model based on the acf/pacf plots. 
#    Call this model uvamcv.kidney.lm2. Analyze the regression results.

uvamcv.kidney.lm2 <- lm(kid.diff[2:29] ~ r11donor$Kidney[2:29] + uva.kidney.lm$residuals[1:28])
kiddiff <- kid.diff[2:29]#saving response variable to a vector for 5.3
residuals <- uva.kidney.lm$residuals[1:28] #saving predictors variable to a vector for 5.3
r11 <- r11donor$Kidney[2:29] #saving predictors variable to a vector for 5.3

summary(uvamcv.kidney.lm2)
  # All coefficients are significant, and the model itself is significant at a <0.001 level.

#Generate diagnostic plots for uvamcv.kidney.lm2. What are your observations?
par(mfrow=c(2,2))
plot(uvamcv.kidney.lm2)
par(mfrow=c(1,1))

#Step 3.3 Bootstrap the above time series model. Are the coefficients significant?

#    Get the fitted values from the regression model
uvamcv.kfit <- fitted(uvamcv.kidney.lm2)

#    Get the residuals from the regression model
uvamcv.ke <- residuals(uvamcv.kidney.lm2)

#    Get the regression model
uvamcv.mod <- model.matrix(uvamcv.kidney.lm2)

#     Use the RTSB function to obtain the bootstrap
uvamcv.boot <- RTSB(uva$Kidney[2:29], r11donor$Kidney[2:29], uvamcv.kfit, uvamcv.ke, uvamcv.mod,2000)

#     The estimates
uvamcv.boot$t
uvamcv.boot$t0


#    Plot the results for the coeffiecient for region 11 donors
plot(uvamcv.boot, index=2)

#    Plot the results for the coeffiecient for time series components

plot(uvamcv.boot, index=3)

#    Are the coefficients significant?
sqrt(abs(var(uvamcv.boot$t)))
boot.ci(uvamcv.boot, .95, index=2)

  # 0 is not included in the confidence intervals, so the r11donor predictor is significant

boot.ci(uvamcv.boot, .95, index=3)

  # 0 is not included in the confidence interval, so the coefficient AR(1) component is significant

#Step 3.5* (bonus) What about Duke? Repeat the above steps and compare the results. 
#Step 3.1 (Duke) Generate the ACF and PACF plots of the residuals from your part 2 linear model for UVA and Duke kidney transplants. Interpret the results of the ACF and PACF plots. Do you recommend modeling the residuals? If so, what kind of model should you try based on these plots?  
par(mfrow = c(1,2))
acf(uvaduke.kidney.lm$residuals)
pacf(uvaduke.kidney.lm$residuals)
par(mfrow = c(1,1))
# lags are significant, we should use an AR model with a lag of 2

#Step 3.2 (Duke) Based on the above ACF and PACF plots, fit an ar model to the residuals

#    Add the AR model of the residuals to regression linear model based on the acf/pacf plots. 
#    Call this model uvaduke.kidney.lm2. Analyze the regression results.

uvaduke.kidney.lm2 <- lm(kid.diff2[2:29] ~ r11donor$Kidney[2:29] + uvaduke.kidney.lm$residuals[1:28])
kiddiff2 <- kid.diff2[2:29]#saving response variable to a vector for 5.3
residuals <- uvaduke.kidney.lm$residuals[1:28] #saving predictors variable to a vector for 5.3
r11 <- r11donor$Kidney[2:29] #saving predictors variable to a vector for 5.3

summary(uvaduke.kidney.lm2)
# Model is significant at p=0.003132. r11donor$Kidney coeff significant at 0.05, uvaduke.kidney.lm$residuals coeff significant at 0.001

#Generate diagnostic plots for uvaduke.kidney.lm2. What are your observations?
par(mfrow=c(2,2))
plot(uvaduke.kidney.lm2)
par(mfrow=c(1,1))
#better diagnostics than previous model but not perfect

#Step 3.3 (Duke) Bootstrap the above time series model. Are the coefficients significant?

#    Get the fitted values from the regression model
uvaduke.kfit2 <- fitted(uvaduke.kidney.lm2)

#    Get the residuals from the regression model
uvaduke.ke2 <- residuals(uvaduke.kidney.lm2)

#    Get the regression model
uvaduke.mod2 <- model.matrix(uvaduke.kidney.lm2)

#     Use the RTSB function to obtain the bootstrap
uvaduke.boot2 <- RTSB(uva$Kidney[2:29], r11donor$Kidney[2:29], uvaduke.kfit2, uvaduke.ke2, uvaduke.mod2,2000)

#     The estimates
uvaduke.boot2$t
uvaduke.boot2$t0


#    Plot the results for the coeffiecient for region 11 donors
plot(uvaduke.boot2, index=2)

#    Plot the results for the coeffiecient for time series components

plot(uvaduke.boot2, index=3)

#    Are the coefficients significant?
sqrt(abs(var(uvaduke.boot2$t)))
boot.ci(uvaduke.boot2, .95, index=2)
#(-0.0553, -0.0023). Doesn't contain 0, so coefficient is significant


boot.ci(uvaduke.boot2, .95, index=3)

#  (0.1889,  0.8112). 0 is not included in the confidence interval, so the coefficient AR(1) component is significant
#end duke

#***************************************************************
#
# Part 4: Predicting Differences in Kidney Transplants Part 1
#
#***************************************************************

#Step 4.1 Build an AR model to predict the difference in 2017

uvamcv.kid.diff.ar <- ar(kid.diff)

#Step 4.2 Use the predict function with ar model to forecast 2017 differences between UVA and MCV

# Calculate the CI and plot the time series and prediction and CIs on a graph

uvamcv.pred <- predict(uvamcv.kid.diff.ar)
upper_CI <- uvamcv.pred$pred[1]+1.96*uvamcv.pred$se[1]
lower_CI <- uvamcv.pred$pred[1]-1.96*uvamcv.pred$se[1]

# Plot the historical time series, the new prediction, and CIs on a graph

plot(kid.diff,ylim=c(-85,25),xlim=c(1987,2018))
points(x=2017,y=uvamcv.pred$pred)
segments(2017,uvamcv.pred$pred,2017,upper_CI)
segments(2017,uvamcv.pred$pred,2017,lower_CI)


# What do you observe?

# A very wide confidence interval for the next value, indicating the time series isn't 
# a great predictor of the next value.

#Step 4.3 Bootstrapping the difference of UVa and MCV in 2017
#    To obtain a bootstrap estimate of the prediction for 2017
#    use the TSB function in the source file.

#    It takes three arguments:
#    tsint - the time series
#    oth.arg - the data for the new estimate
#    boot.number- number of replications (default=1000)

mcvuva.boot4 <- TSB(kid.diff,uvamcv.pred$pred,2000)

#    Interpret the results. Is there a significant difference between UVA and MCV in 2017?

tsboot.ci(mcvuva.boot4)
# there is a significant difference between UVA and MCV for 2017 since the 95% confidence interval does
# not include 0

#Step 4.4* (bonus) What about Duke? Repeat the above steps and compare for Duke.
#Step 4.1 (Duke) Build an AR model to predict the difference in 2017

uvaduke.kid.diff.ar <- ar(kid.diff2)

#Step 4.2 (Duke) Use the predict function with ar model to forecast 2017 differences between UVA and MCV

# Calculate the CI and plot the time series and prediction and CIs on a graph

uvaduke.pred <- predict(uvaduke.kid.diff.ar)
upper_CI <- uvaduke.pred$pred[1]+1.96*uvaduke.pred$se[1]
lower_CI <- uvaduke.pred$pred[1]-1.96*uvaduke.pred$se[1]

# Plot the historical time series, the new prediction, and CIs on a graph

plot(kid.diff2,ylim=c(-85,25),xlim=c(1987,2018))
points(x=2017,y=uvaduke.pred$pred)
segments(2017,uvaduke.pred$pred,2017,upper_CI)
segments(2017,uvaduke.pred$pred,2017,lower_CI)


# What do you observe?

# A very wide confidence interval for the next value, indicating the time series isn't 
# a great predictor of the next value.

#Step 4.3 (Duke) Bootstrapping the difference of UVa and Duke in 2017
#    To obtain a bootstrap estimate of the prediction for 2017
#    use the TSB function in the source file.

#    It takes three arguments:
#    tsint - the time series
#    oth.arg - the data for the new estimate
#    boot.number- number of replications (default=1000)

dukeuva.boot4 <- TSB(kid.diff2,uvaduke.pred$pred,2000)

#    Interpret the results. Is there a significant difference between UVA and MCV in 2017?

tsboot.ci(dukeuva.boot4)
# there is a significant difference between UVA and Duke for 2017 since the 95% confidence interval does
# not include 0
#end duke

#***************************************************************
#
# Part 5: Predicting Differences in Kidney Transplants Part 2
#
#***************************************************************

#Step 5.1 Develop an AR model of region 11 kidney donors

#Plot an acf/pacf to estimate the model order for region 11 kidney donors - what model order do you suggest?
#Use ar() to fit an ar model to region 11 kidney donors from 1988-2016

par(mfrow=c(1,2))
acf(r11donor$Kidney)
pacf(r11donor$Kidney)
par(mfrow=c(1,1))

r11donor.ar <- ar(r11donor$Kidney[1:29])

#Step 5.2 Forecast the R11 donors and standard errors for 2017 using your ar model from step 5.1. Use forecast from the library(forecast).

r11forecast <-  forecast(r11donor.ar,h=1)

#Step 5.3 Use the linear model from part 3.3 combined with the forecast of region 11 kidney donors
#to forecast the differences in number of kidney transplants between UVa and MCV for 2017.
#Use the predict() function

#   Creating the new data frame
  kiddiff <- kid.diff[2:29]#saving response variable to a vector for 5.3
  residuals <- uva.kidney.lm$residuals[1:28] #saving predictors variable to a vector for 5.3
  r11 <- r11donor$Kidney[2:29] #saving predictors variable to a vector for 5.3
df5 <- data.frame(kiddiff, residuals, r11)


#   Predict the linear model with the time series
uvamcv.combined.lm<- lm(kiddiff~., data=df5)
summary(uvamcv.combined.lm)
ndf5 <- data.frame(r11=r11forecast$mean,residuals=uva.kidney.lm$residuals[29])

# Use predict(model from 3.2, newdata, )
df5.pred <- predict(uvamcv.combined.lm, newdata = ndf5, se.fit=TRUE, interval="predict")
df5.pred$fit

#Step 5.4 Bootstrap the Forecast from the linear model combined with the forecast of region 11 kidney donors to forecast the differences in number of kidney transplants between UVa and MCV for 2017.

#   Bootstrap prediction
df5.boot5 <- RFB(df5, model=uvamcv.combined.lm, ndata=ndf5, num=2000)
summary(df5.boot5)
df5.boot5

#   Bootstrap plot 
df5.ci <- boot.ci(df5.boot5, index=1)
df5.ci
lower_CI <- df5.ci$percent[4]
upper_CI <- df5.ci$percent[5]

plot(kid.diff,ylim=c(-85,30),xlim=c(1987,2018))
points(x=2017,y=df5.boot5$t0[1])
segments(2017,df5.boot5$t[1],2017,upper_CI)
segments(2017,df5.boot5$t[1],2017,lower_CI)

#   Bootstrap confidence intervals
boot.ci(df5.boot5,0.95,type=c('bca','perc'))

#   Interpret the results
  # are significantly different - uva has fewer  ( diff was uva - mcv and answers are negative) 

#Step 5.5 Plot the current and predictions for each value along with the confidence intervals. Describe your observations.
plot(kid.diff,ylim=c(-85,30),xlim=c(1987,2018))
points(x=2017,y=df5.boot5$t0[1])
uva17 <- read.table("UVAxplant.csv", sep = ",", header = T)
points(x=2017, uva17$Kidney[30]-mcv$Kidney[30])
segments(2017,df5.boot5$t[1],2017,upper_CI)
segments(2017,df5.boot5$t[1],2017,lower_CI)



#Step 5.6* (bonus) What about Duke? Repeat the above steps for Duke.

#Step 5.1 (Duke) Develop an AR model of region 11 kidney donors

#Plot an acf/pacf to estimate the model order for region 11 kidney donors - what model order do you suggest?
#Use ar() to fit an ar model to region 11 kidney donors from 1988-2016

par(mfrow=c(1,2))
acf(r11donor$Kidney)
pacf(r11donor$Kidney)
par(mfrow=c(1,1))

r11donor.ar <- ar(r11donor$Kidney[1:29])

#Step 5.2 (Duke) Forecast the R11 donors and standard errors for 2017 using your ar model from step 5.1. Use forecast from the library(forecast).

r11forecast <-  forecast(r11donor.ar,h=1)

#Step 5.3 (Duke) Use the linear model from part 3.3 combined with the forecast of region 11 kidney donors
#to forecast the differences in number of kidney transplants between UVa and Duke for 2017.
#Use the predict() function

#   Creating the new data frame
kiddiff2 <- kid.diff2[2:29]#saving response variable to a vector for 5.3
residualsduke <- uvaduke.kidney.lm$residuals[1:28] #saving predictors variable to a vector for 5.3
r11 <- r11donor$Kidney[2:29] #saving predictors variable to a vector for 5.3
df5duke <- data.frame(kiddiff2, residualsduke, r11)


#   Predict the linear model with the time series
uvaduke.combined.lm<- lm(kiddiff2~., data=df5duke)
summary(uvaduke.combined.lm)
ndf5duke <- data.frame(r11=r11forecast$mean,residuals=uvaduke.kidney.lm$residuals[29])

# Use predict(model from 3.2, newdata, )
df5duke.pred <- predict(uvaduke.combined.lm, newdata = df5duke, se.fit=TRUE, interval="predict")
df5.pred$fit

#Step 5.4 Bootstrap the Forecast from the linear model combined with the forecast of region 11 kidney donors to forecast the differences in number of kidney transplants between UVa and MCV for 2017.

#   Bootstrap prediction
df5.boot5 <- RFB(df5, model=uvamcv.combined.lm, ndata=ndf5, num=2000)
summary(df5.boot5)
df5.boot5

#   Bootstrap plot 
df5.ci <- boot.ci(df5.boot5, index=1)
df5.ci
lower_CI <- df5.ci$percent[4]
upper_CI <- df5.ci$percent[5]

plot(kid.diff,ylim=c(-85,30),xlim=c(1987,2018))
points(x=2017,y=df5.boot5$t0[1])
segments(2017,df5.boot5$t[1],2017,upper_CI)
segments(2017,df5.boot5$t[1],2017,lower_CI)

#   Bootstrap confidence intervals
boot.ci(df5.boot5,0.95,type=c('bca','perc'))

#   Interpret the results
# are significantly different - uva has fewer  ( diff was uva - mcv and answers are negative) 

#Step 5.5 Plot the current and predictions for each value along with the confidence intervals. Describe your observations.
plot(kid.diff,ylim=c(-85,30),xlim=c(1987,2018))
points(x=2017,y=df5.boot5$t0[1])
uva17 <- read.table("UVAxplant.csv", sep = ",", header = T)
points(x=2017, uva17$Kidney[30]-mcv$Kidney[30])
segments(2017,df5.boot5$t[1],2017,upper_CI)
segments(2017,df5.boot5$t[1],2017,lower_CI)