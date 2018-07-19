# LOAD DATA
setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")
source("AccidentInput.R")
source("SPM_Panel.R")
source("TestSet.R")
library(lattice)
path <- "/Users/sabreenaabedin/Desktop/class/SYS4021/TrainData"
acts <- file.inputl(path) 
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
totacts <- combine.data(acts, comvar)
rm(comvar)
rm(path)
rm(myfiles)
rm(temp)
# CREATE CASUALTY
totacts$Casualty = totacts$TOTKLD + totacts$TOTINJ
casbox <- boxplot(totacts$Casualty > 0)
xcas <- totacts[totacts$Casualty > casbox$stats[5],]
xcasnd <- xcas[!(duplicated(xcas[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
rm(xcas)

# CREATING LINEAR MODEL #1
uva.pairs(totacts[,c("Casualty","TRNSPD", "TEMP", "HEADEND1", "CARS")]) 

lm.cas1<-lm(Casualty~TRNSPD+CARS+HEADEND1,data=xcasnd) # COULD TOTALLY BE THE WRONG METRICS!!!
summary(lm.cas1)
coef(lm.cas1)

# METRICS
sum(lm.cas1$res^2)
summary(lm.cas1)$adj.r.squared
AIC(lm.cas1) #AIC
AIC(lm.cas1,k=log(nrow(xcasnd))) #BIC


# CREATING LINEAR MODEL #2
lm.cas2<-lm(Casualty~TRNSPD+CARS+HEADEND1+TEMP,data=xcasnd) 
summary(lm.cas2)
coef(lm.cas2)

# METRICS
sum(lm.cas2$res^2)
summary(lm.cas2)$adj.r.squared
AIC(lm.cas2)
AIC(lm.cas2,k=log(nrow(xcasnd)))


# STEPWISE REGRESSION
lm.cas2.step<-step(lm.cas2, trace=T)
summary(lm.cas2.step)

# PARTIAL F TEST
anova(lm.cas2,lm.cas2.step)

# AIC
AIC(lm.cas1) 
AIC(lm.cas2)
AIC(lm.cas2.step)

# BIC
AIC(lm.cas1,k=log(nrow(xcasnd))) 
AIC(lm.cas2,k=log(nrow(xcasnd))) 
AIC(lm.cas2.step,k=log(nrow(xcasnd))) 

# ADJUSTED R SQUARED
summary(lm.cas1)$adj.r.squared
summary(lm.cas2)$adj.r.squared
summary(lm.cas2.step)$adj.r.squared

# TEST SETS
# compare lm.cas2.step and lm.cas1 and lm.cas2

test.size<-1/3
xcasnd.data<-test.set(xcasnd,test.size)
dim(xcasnd.data)

par(mfrow=c(2,2))
hist(xcasnd.data$train$Casualty)
hist(xcasnd.data$test$Casualty)
hist(xcand$Casualty)
par(mfrow=c(1,1))

# COMPARE PMSE

lm.cas1.train<-lm(ACCDMG~TRNSPD+CARS+HEADEND1,data=xcasnd.data$train)
lm.cas1.pred<-predict(lm.cas1.train,newdata=xcasnd.data$test) 
pmse.lm.cas1<-mse(lm.cas1.pred,xcasnd.data$test$ACCDMG)
pmse.lm.cas1

lm.cas2.train<-lm(ACCDMG~TEMP+TRNSPD+CARS+HEADEND1,data=xcasnd.data$train)
lm.cas2.pred<-predict(lm.cas2.train,newdata=xcasnd.data$test) 
pmse.lm.cas2<-mse(lm.cas2.pred,xcasnd.data$test$ACCDMG)
pmse.lm.cas2

