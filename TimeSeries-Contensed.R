#************************************************************
#  			SPAM Filter  
#			Time Series Analysis 
#************************************************************

#*****************************
# Load the data & source files
#*****************************

setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")
ham<-read.table('ham_ts.csv',header=T,sep=',')
spam<-read.table('spam_ts.csv',header=T,sep=',')

summary(ham)
summary(spam)

##use the ts() command to get a time series
ham.ts<-ts(ham$count)
spam.ts<-ts(spam$count)

library(forecast)

#*****************************
# Observations
#     &  
# Model trend, seasonality
#*****************************

plot(spam.ts)

plot(ham.ts)
  
  # ending looks unusal, perform Wilcoxon test of means
  
  # Notched box plots for days 1:464 and days 465:506
  boxplot(ham.ts[1:464], ham.ts[465:506], notch = T, ylab = 'Ham E-mails', names = c('Weeks 1/13/00 - 4/20/01', 'Weeks 4/20/01 - 6/1/01'), main = 'Notched Box Plots for Ham E-mails', col = c('blue', 'red'))

  # Perform a Wilcoxon test of the means for ham.ts for days 1:464 and days 465:506
  wilcox.test(ham.ts[1:464],ham.ts[465:506])

  # Remove the last six weeks from ham.ts
  ham.ts <-ts(ham$count[1:464])

# Plot ACF for HAM - this has daily periodicity 
acf(ham.ts)

# Plot ACF for SPAM - potential weekly periodicity 
acf(spam.ts)

#### MODELING TREND ####

# Model trend of spam based on the time variable
time.spam<-c(1:(length(spam.ts))) # create time variable
spam.trend<-lm(spam.ts~time.spam)
summary(spam.trend)

# Model trend of ham based on time variable
time.ham<-c(1:length(ham.ts))
ham.trend<-lm(ham.ts~time.ham)
summary(ham.trend)

#Plot the trend line for ham.ts
plot(ham.ts)
abline(ham.trend,col='red')

#### MODELING SEASONALITY ####

# Model the seasonality for ham data set using dummy variables, use day of the week as the interval
ham.day <- time.ham %% 7
ham.day <-as.factor(time.ham %% 7) 

Day <- rep(NA, length(ham.ts))
Day[which((time.ham %% 7)    == 1)] <- "Th"  
Day[which((time.ham %% 7)    == 2)] <- "F"
Day[which((time.ham %% 7)    == 3)] <- "Sa"
Day[which((time.ham %% 7)    == 4)] <- "S"
Day[which((time.ham %% 7)    == 5)] <- "M"
Day[which((time.ham %% 7)    == 6)] <- "T"
Day[which((time.ham %% 7)    == 0)] <- "W"
Day <- as.factor(Day)

contrasts(Day)

# Build a model ham.trendseason to model the trend and seasonality of ham.
ham.trendseason<-lm(ham.ts~time.ham+Day)
summary(ham.trendseason) 

#### DIAGNOSTICS ####

# Spam
par(mfrow=c(2,2))
plot(spam.trend, labels.id = NULL)
par(mfrow=c(1,1))

  # lack of fit, but okay

# Ham
par(mfrow=c(2,2))
plot(ham.season, labels.id = NULL)
par(mfrow=c(1,1))

  # lack of fit, two major outliers

##### PERIODOGRAM #####

# Get the periodogram for ham.ts
pg.ham<-spec.pgram(ham.ts,spans=9,demean=T,log='no')

# Find the peak
max.omega.ham<-pg.ham$freq[which(pg.ham$spec==max(pg.ham$spec))] 
max.omega.ham

# Period
1/max.omega.ham

# repeat, for spam
pg.spam<-spec.pgram(spam.ts,spans=9,demean=T,log='no')
max.omega.spam<-pg.spam$freq[which(pg.spam$spec==max(pg.spam$spec))]
max.omega.spam
1/max.omega.spam


# Build a new model, ham.season which predicts ham.ts with a 7 day lag (based on period)
ham.ts.7 = ts.intersect(ham.ts, ham7=lag(ham.ts,-7), dframe=TRUE)
ham.season <- lm(ham.ts~ ham7, data=ham.ts.7)

summary(ham.season) # model is significant

#*****************************
# 
# AR, MA & ARIMA Models  
# 
#*****************************

# Get the residuals from the ham.season model above and store in e.ts.ham:
e.ts.ham <- ts(ham.season$residuals)
plot(e.ts.ham)

acf(e.ts.ham)

pacf(e.ts.ham)

# Plot acf and pacf for e.ts.ham side by side for easier examination
par(mfrow=c(1,2))
acf(e.ts.ham, main="ACF of Residuals from ham.season")
pacf(e.ts.ham,main="PACF of Residuals from ham.season")
par(mfrow=c(1,1))
  # Does this time series still have structure to be modeled? Yes

# Do we need to consider a first order difference of our residuals?
plot(diff(e.ts.ham))

par(mfrow=c(1,2))
acf(diff(e.ts.ham), main="Diff ACF of Residuals from ham.season")
pacf(diff(e.ts.ham),main="Diff PACF of Residuals from ham.season")
par(mfrow=c(1,1))


# Shall we use the first order difference in this case?


# Let us choose p and q terms for e.ts.ham based on the acf and pacf 

# arma(2,1) p=2, q=1
ham.arma21 <- arima(e.ts.ham, order=c(2,0,1))
summary(ham.arma21)
AIC(ham.arma21)

# ar(2) p=2
ham.ar2 <- arima(e.ts.ham, order=c(2,0,0))
summary(ham.ar2)
AIC(ham.ar2)

# arima(2,1,1) p=2, d=1, q=1
ham.arima211 <- arima(e.ts.ham, order=c(2,1,1))
summary(ham.arima211)
AIC(ham.arima211)

# Use the auto.arima from 'forecast' library- Automatically find p, q, & d terms
ham.auto <- auto.arima(e.ts.ham,trace=TRUE)
summary(ham.auto)
AIC(ham.auto)


# REPEAT FOR SPAM

e.ts.spam <- ts(spam.trend$residuals)
plot(e.ts.spam)

par(mfrow=c(1,2))
acf(e.ts.spam, main="ACF of Residuals from spam.trend")
pacf(e.ts.spam,main="PACF of Residuals from spam.trend")
par(mfrow=c(1,1))

plot(diff(e.ts.spam))

par(mfrow=c(1,2))
acf(diff(e.ts.spam), main="Diff ACF of Residuals from spam.trend")
pacf(diff(e.ts.spam),main="Diff PACF of Residuals from spam.trend")
par(mfrow=c(1,1))

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
spam.auto <- auto.arima(e.ts.spam, trace=TRUE, stepwise = FALSE)
summary(spam.auto)
AIC(spam.auto)

# Use the auto.arima from 'forecast' library- Automatically find p, q, & d terms for whole spam time series
spam.auto.whole <- auto.arima(spam.ts, trace=TRUE)
summary(spam.auto.whole)
AIC(spam.auto.whole)

# arma(1,1) p=1, q=1
spam.arma11.whole <- arima(spam.ts, order=c(1,0,1))
summary(spam.arma11.whole)
AIC(spam.arma11.whole)

#************************************************************
#      	SPAM Filter  
#			Time Series Analysis 3
#************************************************************

# Plot diagnostics for models model using tsdiag()

tsdiag(spam.arma11,gof.lag=20)
tsdiag(spam.ar1,gof.lag=20)
tsdiag(spam.arima111,gof.lag=20)
tsdiag(spam.auto,gof.lag=20)
tsdiag(spam.auto1,gof.lag=20)


# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals spam.auto
par(mfrow=c(1,2))
acf(spam.auto$residuals, main="ACF of Residuals from spam.auto")
pacf(spam.auto$residuals,main="PACF of Residuals from spam.auto")
par(mfrow=c(1,1))
  # auto function accounts for correlation in residuals

##### FORECASTING #####

# Use the forecast option to forecast the next 7 days of spam data- spam.auto.forecast
spam.auto.forecast<-forecast(spam.auto,h=7)
plot(spam.auto.forecast)

# PREDICTION

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


#************************************************
#
# Repeat the process for ham
#
#************************************************

#*****************************
# 
# Model Trend and Seasonality  
# 
#*****************************

#Plot ham.ts and acf and pacf of ham.ts


# Create a new variable for the index
# of days in the ham series and hold out 1 week


# Model the trend and seasonality of ham

#*****************************
# 
# AR, MA & ARIMA Models  
# 
#***************************** 

#get the residuals from the ham.lm model above and store in e.ts:


##plot the residuals for the ham.lm model


# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals ham.lm


# Does this time series still have structure to be modeled?


# Do we need to consider a first order difference of our residuals?


# Use the auto.arima from 'forecast' library- Automatically find p, q, & d terms


# Diagnostics



# Which diagnostics plots are adequate?  Which are not?


# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals ham.auto



# Forecasting:

# Use the forecast option to forecast the next 7 days of ham data- ham.auto.forecast



# Prediction performance
# Create test set from ham data set with last 7 days
# the next week or the test period in days


# Prediction for the next week by ham.lm and ham.auto:


# MSE:


# Plot actual values and predicted values and confidence intervals


#**********************
# Another model
# Ham with just ARIMA
#**********************

#Look at acf and pacf of ham.ts[time.ham]


# Using just auto arima


# look at diagnostics



#Look at acf and pacf of residuals of model


# Other models?



# Forecasting, MSE, and plotting prediction performance:


