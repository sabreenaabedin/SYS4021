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
mcv <- read.table("MCVxplant.csv", sep = ",", header = T)
duke <- read.table("Dukexplant.csv", sep = ",", header = T)


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
  # all distributions have bimodal plots

#On average, how many kidney transplants are performed at UVa per year? MCV?
mean(uva$Kidney) # 68.533 ~ 69
mean(mcv$Kidney) # 85.7 ~ 86


#Perform a paired t-test between uva and mcv kidney transplants
#What is the result? p-value is low, significantly different
uva.kidney<-uva$Kidney
mcv.kidney<-mcv$Kidney
t.test(uva$kidney, mcv.kidney,paired=T)



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
#What is the result? low p-value, is significant
uva.kidney<-uva$Kidney
duke.kidney<-duke$Kidney
t.test(uva.kidney, duke.kidney,paired=T) 

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

uvamcv.kidney.lm2 <- ar(uva.kidney.lm$residuals, method = "yule-walker") 


#Generate diagnostic plots for uvamcv.kidney.lm2. What are your observations?
par(mfrow=c(2,2))
plot(uvamcv.kidney.lm2)
par(mfrow=c(1,1))

#Step 3.3 Bootstrap the above time series model. Are the coefficients significant?

#    Get the fitted values from the regression model
uvamcv.kfit <- fitted(uva.kidney.lm)

#    Get the residuals from the regression model
uvamcv.ke <- residuals(uva.kidney.lm)

#    Get the regression model
uvamcv.mod <- model.matrix(uva.kidney.lm)

#     Use the RTSB function to obtain the bootstrap
uvamcv.boot <- RTSB(uva$Kidney[-30], r11donor$Kidney[-30], uvamcv.kfit, uvamcv.ke, uvamcv.mod,2000)

#     The estimates
uvamcv.boot$t
uvamcv.boot$t0


#    Plot the results for the coeffiecient for region 11 donors
plot(uvamcv.boot, index=2)

#    Plot the results for the coeffiecient for time series components


#    Are the coefficients significant?
sqrt(abs(var(uvamcv.boot$t)))
boot.ci(uvamcv.boot, .95, index=2)

#Step 3.5* (bonus) What about Duke? Repeat the above steps and compare the results. 


#***************************************************************
#
# Part 4: Predicting Differences in Kidney Transplants Part 1
#
#***************************************************************

#Step 4.1 Build an AR model to predict the difference in 2017



#Step 4.2 Use the predict function with ar model to forecast 2017 differences between UVA and MCV


# Calculate the CI and plot the time series and prediction and CIs on a graph

# Plot the historical time series, the new prediction, and CIs on a graph

# What do you observe?


#Step 4.3 Bootstrapping the difference of UVa and MCV in 2017
#    To obtain a bootstrap estimate of the prediction for 2017
#    use the TSB function in the source file.

#    It takes three arguments:
#    tsint - the time series
#    oth.arg - the data for the new estimate
#    boot.number- number of replications (default=1000)


#    Interpret the results. Is there a significant difference between UVA and MCV in 2017?



#Step 4.4* (bonus) What about Duke? Repeat the above steps and compare for Duke.


#***************************************************************
#
# Part 5: Predicting Differences in Kidney Transplants Part 2
#
#***************************************************************

#Step 5.1 Develop an AR model of region 11 kidney donors

#Plot an acf/pacf to estimate the model order for region 11 kidney donors - what model order do you suggest?


#Use ar() to fit an ar model to region 11 kidney donors from 1988-2016



#Step 5.2 Forecast the R11 donors and standard errors for 2017 using your ar model from step 5.1. Use forecast from the library(forecast).



#Step 5.3 Use the linear model from part 3.3 combined with the forecast of region 11 kidney donors
#to forecast the differences in number of kidney transplants between UVa and MCV for 2017.
#Use the predict() function

#   Creating the new data frame



#   Predict the linear model with the time series


#Step 5.4 Bootstrap the Forecast from the linear model combined with the forecast of region 11 kidney donors to forecast the differences in number of kidney transplants between UVa and MCV for 2017.

#   Bootstrap prediction


#   Bootstrap plot 


#   Bootstrap confidence intervals


#   Interpret the results


#Step 5.5 Plot the current and predictions for each value along with the confidence intervals. Describe your observations.


#Step 5.6* (bonus) What about Duke? Repeat the above steps for Duke.