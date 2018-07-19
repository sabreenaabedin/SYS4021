setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")
ham<-read.table('ham_ts.csv',header=T,sep=',')
spam<-read.table('spam_ts.csv',header=T,sep=',')

summary(spam)
summary(ham)

ham.ts<-ts(ham$count)
spam.ts<-ts(spam$count)

library(forecast)

# model trend
time.spam<-c(1:(length(spam.ts)))
spam.trend<-lm(spam.ts[1:(length(spam.ts))]~time.spam)
summary(spam.trend)

# plot acf
acf(spam.ts)

# remove last 6 weeks
ham.ts <-ts(ham$count[1:464])
time.ham<-c(1:(length(ham.ts)))
ham.trend<-lm(ham.ts[1:(length(ham.ts))]~time.ham)
summary(ham.trend)

acf(ham.ts)

# trend + seasonality
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

ham.trendseason<-lm(ham.ts~time.ham+Day)
summary(ham.trendseason) 

# seasonality only
ham.season<-lm(ham.ts~ham.day)
summary(ham.season) 

# comparison
anova(ham.season, ham.trendseason)
