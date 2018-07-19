pg.spam<-spec.pgram(spam.ts)

e.111 <- ts(spam.auto$residuals)
par(mfrow=c(1,2))
acf(e.111, main="ACF of Residuals from spam")
pacf(e.111,main="PACF of Residuals from spam")
par(mfrow=c(1,1))

spam7 <- lag(spam.ts, -7)
spam.ts.7 = ts.intersect(spam.ts, spam7, dframe=TRUE)
spam.ts.auto <- lm(spam.ts~ spam7, data=spam.ts.7)


spam.auto1 <- auto.arima(spam7[time.spam], trace=TRUE)
spam.auto1
