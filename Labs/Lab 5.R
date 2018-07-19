setwd("/Users/sabreenaabedin/Desktop/class/SYS4021") #SABREENA'S
source("AccidentInput.R")
source("SPM_Panel.R")
source("TestSet.R")
library(lattice)

path <- "/Users/sabreenaabedin/Desktop/class/SYS4021/TrainData" #SABREENA'S
acts <- file.inputl(path) 
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
totacts <- combine.data(acts, comvar)
rm(comvar)
totacts <- totacts[!duplicated(totacts[,c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]),]
totacts$Casualty <- totacts$TOTINJ + totacts$TOTKLD
totacts$Cause <- rep(NA, nrow(totacts))
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"
totacts$Cause <- factor(totacts$Cause)
totacts$Type <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))


dmgbox <- boxplot(totacts$ACCDMG)
length(dmgbox$out)
dmgbox$stats
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

# RUN DIAGNOSTICS

accdmg.lm1<-lm(ACCDMG~(TEMP+ TRNSPD+ HEADEND1+CARS) ,data=xdmg)
par(mfrow=c(2,2))
plot(accdmg.lm1, labels.id = NULL)
par(mfrow=c(1,1)) 


plot(accdmg.lm1,which=1) #Residual vs. Fitted
plot(accdmg.lm1,which=2) #QQ
plot(accdmg.lm1,which=3) #Scale-Location
plot(accdmg.lm1,labels.id = NULL, which=4) #Cook's distance
plot(accdmg.lm1,which=5) #Redisuals vs. Leverage
plot(accdmg.lm1,which=6) #Cook's dist vs. Leverage

xdmg[5568,"Type"]
max(xdmg[,"ACCDMG"])
xdmg[5568,"ACCDMG"]

# TRANSFORM RESPONSE VARIABLE

library(MASS)

boxcox(accdmg.lm1) #box-cox plot
boxcox(accdmg.lm1, plotit=T, lambda=seq(-2,2,by=0.5))
boxcox(accdmg.lm1,plotit=F) #values
# find y value for maximum lambda
max(boxcox(accdmg.lm1, plotit = F)$y)
# find best lamda value
boxcox(accdmg.lm1, plotit = F)$x[which.max(boxcox(accdmg.lm1, plotit = F)$y)] 
#get the best lambda and store in L
L<-boxcox(accdmg.lm1, plotit = F)$x[which.max(boxcox(accdmg.lm1, plotit = F)$y)] 
#model with the best lambda
accdmg.lm1.boxcox<-lm(ACCDMG^L ~TEMP+TRNSPD+CARS+HEADEND1,data=xdmg)
summary(accdmg.lm1.boxcox)


par(mfrow=c(2,2))
plot(accdmg.lm1.boxcox, labels.id = NULL)
par(mfrow=c(1,1)) 

plot(accdmg.lm1.boxcox,which=1) #Residual vs. Fitted
plot(accdmg.lm1.boxcox,which=2) #QQ
plot(accdmg.lm1.boxcox,which=3) #Scale-Location
plot(accdmg.lm1.boxcox,labels.id = NULL, which=4) #Cook's distance
plot(accdmg.lm1.boxcox,which=5) #Redisuals vs. Leverage
plot(accdmg.lm1.boxcox,which=6) #Cook's dist vs. Leverage

accdmg.lm2<-lm(ACCDMG~as.factor(WEATHER),data=xdmg)
summary(accdmg.lm2) #What is the base case?


accdmg.lm3<-lm(ACCDMG~as.factor(TYPE),data=xdmg)
accdmg.lm3
summary(accdmg.lm3)
xdmg$TYPE <- as.factor(xdmg$TYPE)
contrasts(xdmg$TYPE)

xdmg$Derail <- rep(NA, nrow(xdmg))
xdmg$Derail[which(xdmg$TYPE == 1)] <- 1
xdmg$Derail[which(!xdmg$TYPE == 1)] <- 0
xdmg$Derail
accdmg.lm4<-lm(ACCDMG~Derail,data=xdmg)
summary(accdmg.lm4)
accdmg.lm4
