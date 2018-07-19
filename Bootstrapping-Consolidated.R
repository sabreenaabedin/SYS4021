#***************************************************************
#     Transplant Center 
#  		Observational Analysis
#***************************************************************

#***************************
# Load the transplant data
#***************************

setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")
source("TSbootfunctions.R")
library(boot)
source("SPM_Panel.R")
source("Transplant.plots.R")

r11xplant <- read.table("R11xplant.csv", sep = ",", header = T)
r11donor<-read.table("R11donor.csv", sep = ",", header = T)
uva <- read.table("UVAxplant.csv", sep = ",", header = T)
duke <- read.table("Dukexplant.csv", sep = ",", header = T)
mcv <- read.table("MCVxplant.csv", sep = ",", header = T)
unc <- read.table("UNCxplant.csv", sep = ",", header = T)

#  How many years of transplant data are there?
nrow(r11xplant)
  # 30 - 1 header = 29 years
#  How many organs? What type of organs? 6
 
#*********************
# Scatter plot matrix 
#*********************

# Create a scatter plot matrix for liver transplants using UVA, Duke, MCV, UNC, & Region 11 donors
liver<-data.frame(uva$Liver,duke$Liver,mcv$Liver,unc$Liver, r11donor$Liver)
uva.pairs(as.matrix(liver))

# Create a scatter plot matrix for pancreas transplants
pancreas<-data.frame(uva$Pancreas,duke$Pancreas,mcv$Pancreas,unc$Pancreas, r11donor$Pancreas)
uva.pairs(as.matrix(pancreas))


#*****************
#  donortype.plot 
#*****************
# remove the 30th observation (2017) since the data were not complete for that year
# DD means deceased donor; LD means living donor

donortype.plot(cbind(r11xplant$Lung_DD[-30], r11xplant$Lung_LD[-30], r11donor$Lung_DD[-30],r11donor$Lung_LD[-30]), title = "Lung")
donortype.plot(cbind(r11xplant$Heart_DD[-30], r11xplant$Heart_LD[-30], r11donor$Heart_DD[-30],r11donor$Heart_LD[-30]), title = "Heart")
donortype.plot(cbind(r11xplant$Liver_DD[-30], r11xplant$Liver_LD[-30], r11donor$Liver_DD[-30],r11donor$Liver_LD[-30]), title = "Liver")

#****************
#  region.plot
#****************

region.plot(cbind(r11xplant$Heart[-30], r11donor$Heart[-30], uva$Heart[-30], unc$Heart[-30], mcv$Heart[-30], duke$Heart[-30]), title = "Heart")
region.plot(cbind(r11xplant$Liver[-30], r11donor$Liver[-30], uva$Liver[-30], unc$Liver[-30], mcv$Liver[-30], duke$Liver[-30]), title = "Liver")
region.plot(cbind(r11xplant$Kidney[-30], r11donor$Kidney[-30], uva$Kidney[-30], unc$Kidney[-30], mcv$Kidney[-30], duke$Kidney[-30]), title = "Kidney")
region.plot(cbind(r11xplant$Pancreas[-30], r11donor$Pancreas[-30], uva$Pancreas[-30], unc$Pancreas[-30], mcv$Pancreas[-30], duke$Pancreas[-30]), title = "Pancreas")
region.plot(cbind(r11xplant$Lung[-30], r11donor$Lung[-30], uva$Lung[-30], unc$Lung[-30], mcv$Lung[-30], duke$Lung[-30]), title = "Lung")

#***************
#  center.plot
#***************

center.plot(cbind( uva$Pancreas[-30], unc$Pancreas[-30], mcv$Pancreas[-30], duke$Pancreas[-30]), title = "Pancreas")
center.plot(cbind( uva$Heart[-30], unc$Heart[-30], mcv$Heart[-30], duke$Heart[-30]), title = "Heart")
center.plot(cbind( uva$Kidney[-30], unc$Kidney[-30], mcv$Kidney[-30], duke$Kidney[-30]), title = "Kidney")
center.plot(cbind( uva$Liver[-30], unc$Liver[-30], mcv$Liver[-30], duke$Liver[-30]), title = "Liver")
center.plot(cbind( uva$All_Organs[-30], unc$All_Organs[-30], mcv$All_Organs[-30], duke$All_Organs[-30]), title = "All Organs")

#***************************************************************
#  	  Transplant Center 
#			Bootstrapping 1
#***************************************************************

#************************************
# Part 1 - Bootstrap the differences
#************************************

# UVA-MCV 
uva.kidney<-uva$Kidney
mcv.kidney<-mcv$Kidney

# Compute the difference between uva kidney transplants and mcv kidney transplants from 1988 to 2016
kid.diff<-ts(uva.kidney-mcv.kidney,1988,2016)
ts.plot(kid.diff,ylab='UVa-MCV',main = "Difference in Number of Transplants, UVA-MCV")

# Perform a paired t-test - shows significantly different
t.test(uva.kidney, mcv.kidney,paired=T)

# boot() resamples based on your chosen statistic
# chose the mean
bs.mean<-function(x,i) { return(mean(x[i])) }

# Bootstrap mean differences - syntax: boot(data= , statistic= , R= ), R = # replications
bs.kid.diff<-boot(kid.diff,bs.mean,R=2000)
bs.kid.diff
  # original = regular t-test results
  # Bias = difference between the mean of the 500 stored bootstrap samples and the original estimate
  # std. error = standard deviation of the 2000 bootstrap samples and is an estimate of the standard error.

plot(bs.kid.diff,index=1) 

# confidence intervals using bca and percentile
boot.ci(bs.kid.diff,0.95,type=c('bca','perc'))

#**********************************************
# Part 2- Bootstrap Regression and Time Series
#**********************************************

# uva$Liver=b0+b1*r11donor$Liver+e..
uva.liver.lm<-lm(uva$Liver[-30]~r11donor$Liver[-30])
summary(uva.liver.lm)

# Diagnostics
par(mfrow=c(2,2))
plot(uva.liver.lm)
par(mfrow=c(1,1))
  # residuals vs variance has non-constant variance, not centered arond 0
  # q-q plot is not normal
  # has high cook's distance

# BOOTSTRAPPING BY RESIDUALS

# Get the fitted values from the regression model
uva.lfit <- fitted(uva.liver.lm)

# Get the residuals from the regression model
uva.le <- residuals(uva.liver.lm)

# Get the regression model - parameter estimates for model
uva.mod <- model.matrix(uva.liver.lm)

# Bootstrapping LM 
uva.liver.boot <- RTSB(uva$Liver[-30], r11donor$Liver[-30], uva.lfit, uva.le, uva.mod,5000)
  # outcome variable, input variable, fitted, residuals, parameters, # replications
uva.liver.boot$t
sqrt(abs(var(uva.liver.boot$t)))

# 95% CI of r11donor
boot.ci(uva.liver.boot, .95, index=2)

# Distribution of b1
par(mfrow = c(1,2))
hist(uva.liver.boot$t[,2], main = "Region 11 Donors",xlab ="Coefficient Values",   col = "steelblue", breaks = 50)
qqnorm(uva.liver.boot $t[,2])
qqline(uva.liver.boot $t[,2])
par(mfrow = c(1,1))
  #looks much better

#***************************************************************
# Bootstrap Regression and Time Series 3
#***************************************************************

# Build a linear model, uva.kid.lm that predicts uva kidney transplants by region 11 kidney donors from 1988-2016
uva.kid.lm <- lm(uva$Kidney[-30]~r11donor$Kidney[-30])
summary(uva.kid.lm) # significant

# Diagnostics
par(mfrow=c(2,2))
plot(uva.kid.lm)
par(mfrow=c(1,1)) # not great

#  BOOTSTRAPPING LINEAR MODEL

# fitted
uva.kfit <- fitted(uva.kid.lm)

# residuals
uva.ke <- residuals(uva.kid.lm)

# regression parameters
uva.mod <- model.matrix(uva.kid.lm)

# boostrap coefficients for distribution
uva.kid.boot <- RTSB(uva$Kidney[-30], r11donor$Kidney[-30], uva.kfit, uva.ke, uva.mod,2000)
uva.kid.boot

summary(uva.kid.lm)

# Get the 99% CI for uva.kid.boot
boot.ci(uva.kid.boot, .99)

#	Plot the results for the coeffiecient for region 11 donors
plot(uva.kid.boot, index = 2) 

#	A set of configurable plots
par(mfrow = c(1,2))
hist(uva.kid.boot$t[,2], main = "Region 11 Donors",xlab ="Coefficient Values",   col = "steelblue", breaks = 50)
qqnorm(uva.kid.boot$t[,2])
qqline(uva.kid.boot$t[,2])
par(mfrow = c(1,1))

#  BOOTSTRAPPING TIME SERIES

# Evaluating residual correlation from the model uva.kid.lm
# Hint: use the acf() and pcf()

par(mfrow = c(1,2))
acf(uva.kid.lm$residuals)
pacf(uva.kid.lm$residuals)
par(mfrow = c(1,1))

#	Fit an ar model to the residuals using the yule-walker method
diff.ar.kid <- ar(uva.kid.lm$residuals, method = "yule-walker") 

# How many autoregressive terms are needed?
diff.ar.kid

# If we use diff.ar.kid
uva.kid.lm2<- lm(uva$Kidney[2:29]~r11donor$Kidney[2:29] + uva.kid.lm$residuals[1:28])
summary(uva.kid.lm2)

# The problem here is we have only a few observations (28)
par(mfrow=c(2,2))
plot(uva.kid.lm2)
par(mfrow=c(1,1))


######## Repeat for liver transplants
# Build a linear model to predict uva liver transplants in terms of region 11 donors

# Model significance?

# Generate the diagnostic plots. Do you see any problems?

# Estimate the liver model with bootstrapping (by residuals). Is b1 significant?

#   Bootstrapping LM
# What is the 95% CI of r11donor?

# Plot the distribution of B1

# Time series models for residuals of liver model

# Generate the ACF and PACF plots of the residuals from uva.liver.lm. What's your conclusion?  

# Fit an ar model to the residuals, what order do you select?

# Bootstrap your time series model. Are the coefficients significant?

# Plot the results for the coeffiecient for region 11 donors and time series components

# What are the confidence intervals for each of the parameters?


