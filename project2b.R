#**********************************
#
# SET UP: LOAD FILES & CLEAN DATA
#
#**********************************

# set working directory 
####### MUST BE CHANGED FOR EACH USER ########
setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")

# load libraries/source files
library(forecast)

# load data
ham<-read.table('ham_ts.csv',header=T,sep=',')
spam<-read.table('spam_ts.csv',header=T,sep=',')

# use the ts() command to get a time series
ham.ts<-ts(ham$count)
spam.ts<-ts(spam$count)

# Remove the last six weeks from ham.ts based on Wilcoxon
wilcox.test(ham.ts[1:464],ham.ts[465:506])
ham.ts <-ts(ham$count[1:464])

# Remove last week of spam.ts - were outliers 
spam.ts <-ts(spam$count[1:358])

# Model trend of SPAM based on the time variable
time.spam<-c(1:(length(spam.ts))) # create time variable
spam.trend<-lm(spam.ts~time.spam)

# Model trend of HAM based on time variable
time.ham<-c(1:length(ham.ts)) # create time variable
ham.trend<-lm(ham.ts~time.ham)

# Create day of week variable
ham.day <- time.ham %% 7
ham.day <-as.factor(time.ham %% 7) 

HDay <- rep(NA, length(ham.ts)) 
HDay[which((time.ham %% 7)    == 1)] <- "Th"  
HDay[which((time.ham %% 7)    == 2)] <- "F"
HDay[which((time.ham %% 7)    == 3)] <- "Sa"
HDay[which((time.ham %% 7)    == 4)] <- "S"
HDay[which((time.ham %% 7)    == 5)] <- "M"
HDay[which((time.ham %% 7)    == 6)] <- "T"
HDay[which((time.ham %% 7)    == 0)] <- "W"
HDay <- as.factor(HDay)

# Create day of week variable
spam.day <- time.spam %% 7
spam.day <-as.factor(time.spam %% 7) 

SDay <- rep(NA, length(spam.ts)) 
SDay[which((time.spam %% 7)    == 1)] <- "Th"  
SDay[which((time.spam %% 7)    == 2)] <- "F"
SDay[which((time.spam %% 7)    == 3)] <- "Sa"
SDay[which((time.spam %% 7)    == 4)] <- "S"
SDay[which((time.spam %% 7)    == 5)] <- "M"
SDay[which((time.spam %% 7)    == 6)] <- "T"
SDay[which((time.spam %% 7)    == 0)] <- "W"
SDay <- as.factor(SDay)

#**********************************
#
# HAM TIME SERIES ANALYSIS
#
#**********************************

####### PART A #######
# Perform a graphical analysis of ham.ts - use time series plots
# periodograms, autocorrelation, and partial autocorrelation plots

# Time Series Plot
plot(ham.ts)
abline(ham.trend,col='red')

# Autocorrelation and Partial Autocorrelation Plots
par(mfrow=c(1,2))
acf(ham.ts, main="ACF ham.ts")
pacf(ham.ts,main="PACF ham.ts")
par(mfrow=c(1,1))

# Periodograms - determine lag
pg.ham<-spec.pgram(ham.ts,spans=9,demean=T,log='no')
max.omega.ham<-pg.ham$freq[which(pg.ham$spec==max(pg.ham$spec))] 
max.omega.ham
1/max.omega.ham # period

####### PART B #######
# Describe how the results from your graphical analysis inform
# your model choice

# The time series shows little/no trend over time
# The ACF shows sinusoidal decay and the PACF cuts off after lag = 15
# this suggests we use an AR model with p = 15
# The ACF and trend line suggest that the data is stationary
#Since both ACF and PACF are sinusoidal, we use an ARMA model 
#Since stationary, use ARMA, not ARIMA

# The ACF and PACF show seasonality and the periodogram shows a period of about 7
# This implies that an AR model should be used to account for seasonality 
# and a MA  ?????? WHEN DO WE USE MOVING AVERAGE?
# model should be used to account for the period. 

####### PART C #######
# Based on these, build and evaluate models to predict ham emails
# Assess models, diagnose problems, and make adjustments
# one model should be chosen, one model should be automated selection

# Model 1: MAIN MODEL = TREND (time.ham) + SEASONALITY (Day)
ham.main<-lm(ham.ts~time.ham+HDay)
summary(ham.main) # p-value < 0.05, model is significant (note: weekends are significant)

# get residuals
e.main <- ts(ham.main$residuals)
plot(e.main)
abline(ham.main,col='red')

# ACF & PACF - implies we need to continue tweaking the model 
par(mfrow=c(1,2))
acf(e.main, main="ACF of Residuals from ham.main")
pacf(e.main,main="PACF of Residuals from ham.main")
par(mfrow=c(1,1))

# arma(2,7) p=2, q=7
ham.arma27 <- arima(e.main, order=c(2,0,7))
summary(ham.arma27)
AIC(ham.arma27)

# Diagnostics
tsdiag(ham.arma27,gof.lag=20)

# Model 2: auto arima based on trend + seasonality model
# Auto ARIMA (from forecast library)
ham.auto <- auto.arima(e.main,trace=TRUE)
summary(ham.auto)
AIC(ham.auto)

# Diagnostics
tsdiag(ham.auto,gof.lag=20)

# Model 3: based on lag
ham.ts.7 = ts.intersect(ham.ts, ham7=lag(ham.ts,-7), dframe=TRUE)
ham.season <- lm(ham.ts~ ham7, data=ham.ts.7)

# get residuals
e.ts.ham <- ts(ham.season$residuals)
plot(e.ts.ham)

# ACF & PACF - implies we need to continue tweaking the model 
par(mfrow=c(1,2))
acf(e.ts.ham, main="ACF of Residuals from ham.season")
pacf(e.ts.ham,main="PACF of Residuals from ham.season")
par(mfrow=c(1,1))

# Auto ARIMA (from forecast library)
ham.lag <- auto.arima(e.ts.ham,trace=TRUE)
summary(ham.lag)
AIC(ham.lag)

# Diagnostics
tsdiag(ham.lag,gof.lag=20)


####### PART D #######
# Which model do you recommend and why?

# Choose the lag model, the Ljung-Box statistic becomes significant at high lags
# if comparing two models with AIC, both have to have the same difference taken - otherwise not on the same time series

#**********************************
#
# SPAM TIME SERIES ANALYSIS
#
#**********************************

####### PART A #######
# Perform a graphical analysis of spam.ts - use time series plots
# periodograms, autocorrelation, and partial autocorrelation plots

# Time Series Plot
plot(spam.ts)
abline(spam.trend,col='red')

# Periodogram - determines lag
pg.spam<-spec.pgram(spam.ts,spans=9,demean=T,log='no')
max.omega.spam<-pg.spam$freq[which(pg.spam$spec==max(pg.spam$spec))]
max.omega.spam
1/max.omega.spam # period

# Autocorrelation and Partial Autocorrelation Plots
par(mfrow=c(1,2))
acf(spam.ts, main="ACF spam.ts")
pacf(spam.ts,main="PACF spam.ts")
par(mfrow=c(1,1))


####### PART B #######
# Describe how the results from your graphical analysis inform
# your model choice

# TIME SERIES: non stationary
# Periodogram: gives a period of 360 which doesn't add value to our data - not enough points
# ACF & PACF: q = 1, p = 3, tell us there is more to be modeled
# Because it's non stationary, we have to use ARIMA instead of ARMA


####### PART C #######
# Based on these, build and evaluate models to predict ham emails
# Assess models, diagnose problems, and make adjustments
# one model should be chosen, one model should be automated selection

# Model 1: Automated Selection

spam.main<-lm(spam.ts~time.spam)
summary(spam.main) # p-value < 0.05, trend is significant 

# get residuals
e.spam.main <- ts(spam.main$residuals)
plot(e.spam.main)

# Use the auto.arima from 'forecast' library- Automatically find p, q, & d terms for whole spam time series
spam.auto <- auto.arima(e.spam.main, trace=TRUE)
summary(spam.auto)
AIC(spam.auto)

# plot diagnostics
tsdiag(spam.auto,gof.lag=20)


# Model 2: Chosen parameters

# Differences - chose a difference of 7
par(mfrow=c(1,2))
acf(diff(e.spam.main), main="Diff ACF of Residuals")
pacf(diff(e.spam.main),main="Diff PACF of Residuals")
par(mfrow=c(1,1))

# arma(1,7,2) p=1, q=2, d = 7
spam.arima <- arima(e.spam.main, order=c(1,7,2))
summary(spam.arima)
AIC(spam.arima)

# plot diagnostics
tsdiag(spam.arima,gof.lag=20)



####### PART D #######
# Which model do you recommend and why?
#### PACF SHOWED DIFFERENCE OF 7 - WHY?????#######

#**********************************
#
# INTEGRATED FILTER DESIGN
#
#**********************************

# Use Baye's Rule to derive the equation for an integrated spam filter
# that combines your static and time series filters. Define all parameters


