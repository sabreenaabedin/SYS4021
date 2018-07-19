#      		Session 7, 8
#			
#	 Multiple Linear Regression 2
#
#******************************************************

setwd("/Users/sabreenaabedin/Desktop/class/SYS4021") #SABREENA'S
source("AccidentInput.R")
source("SPM_Panel.R")
source("PCAplots.R")
source("TestSet.R")
library(lattice)
path <- "/Users/sabreenaabedin/Desktop/class/SYS4021/TrainData" #SABREENA'S
acts <- file.inputl(path) 
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
totacts <- combine.data(acts, comvar)
rm(comvar)

##Build a data frame xdmg with only extreme accidents for ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]


# Setup categorical variables
xdmgnd$Cause <- rep(NA, nrow(xdmgnd))
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "M")] <- "M"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "T")] <- "T"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "S")] <- "S"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "H")] <- "H"
xdmgnd$Cause[which(substr(xdmgnd$CAUSE, 1, 1) == "E")] <- "E"
xdmgnd$Cause <- factor(xdmgnd$Cause)

xdmgnd$Type <- factor(xdmgnd$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

#***********************************************************
#  	Possible predictors of damage	
#***********************************************************

# SPM
uva.pairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])

# PCA
pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )
biplot(pred.pca)


###############################
# Categorical plots

# heatmap
source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xdmgnd$Cause, xdmgnd$Type), title = "No. of Accidents by Cause and Type of Accident")


## INTERACTION PLOTS 

# log of response
xyplot(log(ACCDMG)~TRNSPD | Type, data = xdmgnd, type = c("p", "r"))
xyplot(log(ACCDMG)~TRNSPD | Cause, data = xdmgnd, type = c("p", "r"))

# Cause X Type and TRNSPD
xyplot(log(ACCDMG)~TRNSPD | Cause * Type, data = xdmgnd, type = c("p", "r"))

# plot xy with interactions of Derail and Cause
xdmgnd$Derail <- (xdmgnd$Type == "Derailment")
xyplot(log(ACCDMG)~TRNSPD | Cause * Derail, data = xdmgnd, type = c("p", "r"))

xdmgnd$Freight <- (xdmgnd$TYPEQ == 1)
interaction.plot(xdmgnd$Freight, xdmgnd$Cause, xdmgnd$ACCDMG)

interaction.plot(xdmgnd$Derail, xdmgnd$Cause, log(xdmgnd$ACCDMG))

# Interaction plots with quantitative variables
Speed <- cut(xdmgnd$TRNSPD, c(min(xdmgnd$TRNSPD),50,max(xdmgnd$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))
Cars <- cut(xdmgnd$CARS, c(min(xdmgnd$CARS),5,max(xdmgnd$CARS)), include.lowest = T, labels = c("low hzd", "high hzd"))

interaction.plot(Speed, Cars, log(xdmgnd$ACCDMG))
interaction.plot(xdmgnd$Freight, Speed, log(xdmgnd$ACCDMG))
interaction.plot(xdmgnd$Derail, Speed, log(xdmgnd$ACCDMG))


##******************************************* 
## Build linear regression models in R: lm ##
##*******************************************

# Linear regression models with quantitative predictors

xdmgnd.lm1<-lm(ACCDMG~TEMP,data=xdmgnd)
xdmgnd.lm2<-lm(ACCDMG~TEMP+TRNSPD,data=xdmgnd)

# The next two lines of R code are equivalent
xdmgnd.lm3<-lm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd)
xdmgnd.lm3<-lm(ACCDMG~.,data=xdmgnd[,c('ACCDMG','TEMP','TRNSPD','CARS')])

summary(xdmgnd.lm1)
summary(xdmgnd.lm2)
summary(xdmgnd.lm3)


names(xdmgnd.lm1)
coef(xdmgnd.lm1)
sum(xdmgnd.lm1$res^2) #sum of r^2

##********************
## Criterion based assessments ##
##********************

summary(xdmgnd.lm1)$adj.r.squared #adj r^2
AIC(xdmgnd.lm1) # AIC
AIC(xdmgnd.lm1,k=log(nrow(xdmgnd))) #BIC



##********************
## Stepwise Regression  ##
##********************

xdmgnd.lm3.step<-step(xdmgnd.lm3)

#to see less steps
xdmgnd.lm3.step<-step(xdmgnd.lm3, trace=F)
summary(xdmgnd.lm3.step)


## Partial F Test ##
## Recall that we can only compare two nested models by partial F test:
anova(xdmgnd.lm1,xdmgnd.lm2)

##********************
## Test Sets      ##
##********************

setwd("/Users/sabreenaabedin/Desktop/class/SYS4021") #SABREENA'S
source("TestSet.R")
test.size<-1/3
xdmgnd.data<-test.set(xdmgnd,test.size)

# Check distribution of ACCDMG of test set, training set:

par(mfrow=c(2,2))
hist(xdmgnd.data$train$ACCDMG)
hist(xdmgnd.data$test$ACCDMG)
hist(xdmgnd$ACCDMG)
par(mfrow=c(1,1))


# Build model with train set:

xdmgnd.lm2.train<-lm(ACCDMG~TEMP+TRNSPD,data=xdmgnd.data$train)
xdmgnd.lm3.train<-lm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd.data$train)


# Recall that we need to measure predicted MSE. 
# First, how to predict with lm models:

xdmgnd.lm2.pred<-predict(xdmgnd.lm2.train,newdata=xdmgnd.data$test) 
xdmgnd.lm3.pred<-predict(xdmgnd.lm3.train,newdata=xdmgnd.data$test)


# Next, compute PMSE:

pmse.xdmgnd.lm2<-mse(xdmgnd.lm2.pred,xdmgnd.data$test$ACCDMG)
pmse.xdmgnd.lm2

pmse.xdmgnd.lm3<-mse(xdmgnd.lm3.pred,xdmgnd.data$test$ACCDMG)
pmse.xdmgnd.lm3

#
#      	     Session 8
#			Multiple Linear Regression
#	 TRANSFORMATIONS
#
#******************************************************

# create vectors to store PMSE
pmse1.result<-NULL; #Two empty vectors to record PMSEs
pmse2.result<-NULL;

for (i in c(1:20)){
  #set test sets size: 
  test.size<-1/3
  # generate training sets and test sets from original data:
  xdmgnd.data<-test.set(xdmgnd,test.size)
  
  # Build model with train set:
  lm1.train<-lm(ACCDMG ~ TEMP + TRNSPD + CARS + HEADEND1,data=xdmgnd.data$train)
  lm2.train<-lm(ACCDMG ~ TEMP + CARS,data=xdmgnd.data$train)
  
  # First, how to predict with lm models:
  lm1.pred<-predict(lm1.train,newdata=xdmgnd.data$test) 
  lm2.pred<-predict(lm2.train,newdata=xdmgnd.data$test) 
  
  # Next, compute PMSE:
  pmse.lm1<-mse(lm1.pred,xdmgnd.data$test$ACCDMG)
  pmse.lm2<-mse(lm2.pred,xdmgnd.data$test$ACCDMG)
  
  # Add the PMSE for this run into your vector to stor PMSE
  pmse1.result<-c(pmse1.result,pmse.lm1)
  pmse2.result<-c(pmse2.result,pmse.lm2)
}

# Compare models based over 20 runs of PMSE
plot(pmse1.result,type='b',col='blue',xlab="Index", ylab="PMSE")
lines(pmse2.result,type='b',col='red')
title(main="Model Comparison Based on PMSE")


# Paired t test:
t.test(pmse1.result,pmse2.result,paired=T)

# Wilcoxon Test:
wilcox.test(pmse1.result,pmse2.result,paired=T)


##********************
## Cross-Validation ##
##********************

library(boot)

xdmgnd.lm2.cv<-glm(ACCDMG~TEMP+TRNSPD,data=xdmgnd)
xdmgnd.lm3.cv<-glm(ACCDMG~TEMP+TRNSPD+CARS,data=xdmgnd)


# Cross-validation:
xdmgnd.lm2.err<-cv.glm(xdmgnd,xdmgnd.lm2.cv,K=10)
xdmgnd.lm2.err$delta

xdmgnd.lm3.err<-cv.glm(xdmgnd,xdmgnd.lm3.cv,K=10)
xdmgnd.lm3.err$delta
#first is raw cross validation estimate of prediction error
#second is adjusted estimate


##************************* 
## Diagnostics Plot      ##
##************************* 
# Generate diagnostics plot one by one
plot(xdmgnd.lm1,labels.id = NULL)

#Plot all four plots together
par(mfrow=c(2,2))
plot(xdmgnd.lm1, labels.id = NULL)
par(mfrow=c(1,1)) 

#Save the above plot as png:
png('diagnostics.png')
par(mfrow=c(2,2))
plot(xdmgnd.lm1, labels.id = NULL)
par(mfrow=c(1,1)) 
dev.off()

#Plot graphs individually
plot(xdmgnd.lm1,which=1) #Residual vs. Fitted
plot(xdmgnd.lm1,which=2) #QQ
plot(xdmgnd.lm1,which=3) #Scale-Location
plot(xdmgnd.lm1,labels.id = NULL, which=4) #Cook's distance
plot(xdmgnd.lm1,which=5) #Redisuals vs. Leverage
plot(xdmgnd.lm1,which=6) #Cook's dist vs. Leverage

#plot response variable
plot(density(xdmg$ACCDMG))

##************************* 
## Box-Cox Plot          ##
##************************* 
# load the MASS library 
library(MASS)
boxcox(xdmgnd.lm1) #box-cox plot
boxcox(xdmgnd.lm1, plotit=T, lambda=seq(-2,2,by=0.5))

#get x and y values without plotting
boxcox(xdmgnd.lm1,plotit=F) #values

# find y value for maximum lambda
max(boxcox(xdmgnd.lm1, plotit = F)$y)

# find best lamda value
boxcox(xdmgnd.lm1, plotit = F)$x[which.max(boxcox(xdmgnd.lm1, plotit = F)$y)] 

#get the best lambda and store in L
L<-boxcox(xdmgnd.lm1, plotit = F)$x[which.max(boxcox(xdmgnd.lm1, plotit = F)$y)] 

#model with the best lambda
xdmgnd.lm1.boxcox<-lm(ACCDMG^L ~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd)

# Display regression results for boxcox model
summary(xdmgnd.lm1.boxcox)

# Let's replot our density function for our response variable ACCDMG
plot(density((xdmgnd$ACCDMG^L)))


#
#      	     Session 9
#			Multiple Linear Regression
#	 Transformations & Qualitative Variables
#
#******************************************************

#  create models with interactions between all of the quantitative predictors ----
xdmgnd.lm3<-lm(ACCDMG~(TEMP+TRNSPD+CARS+HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm3) 

#  I() allows your model to contain normal mathematical sysmbols 
#  Create complete second order model                            
xdmgnd.lm4<-lm(ACCDMG~(TEMP+TRNSPD+CARS+HEADEND1)^2+I(TEMP^2)+I(TRNSPD^2)+I(CARS^2)+I(HEADEND1^2),data=xdmgnd)
summary(xdmgnd.lm4)

# Create a stepwise regression model on xdmgnd.lm4
xdmgnd.lm4.step <- step(xdmgnd.lm4)
summary(xdmgnd.lm4.step)

# partial F test ----
anova(xdmgnd.lm3,xdmgnd.lm4)

# Interaction Plot Example
trnspdbox<-boxplot(xdmgnd$TRNSPD)
TRNSPD.factor<-xdmgnd$TRNSPD
TRNSPD.factor[which(xdmgnd$TRNSPD<50)]<-'low train speed'
TRNSPD.factor[which(xdmgnd$TRNSPD>=50)]<-'high train speed'
TRNSPD.factor <- factor(TRNSPD.factor)


# Create derailment variable
Derail <- rep(0, nrow(xdmgnd))
Derail[which(xdmgnd$TYPE == 1)] <- 1 
Derail <- as.factor(Derail)
contrasts(Derail)

# create an interaction plot for TRNSPD and Derailments
interaction.plot(TRNSPD.factor,Derail, xdmgnd$ACCDMG)

##************************* 
## Qualitative Variables ##
##************************* 

# Lets look at the default treatment coding
contrasts(Cause)

# Write a model to predict ACCDMG in terms of Cause 
xdmgnd.lm5<-lm(ACCDMG~Cause,data=xdmgnd)
summary(xdmgnd.lm5) 

#Chage based case to H ----
contrasts(Cause)<-matrix(c(1,0,0,0,0, 0,0,1,0,0, 0,0,0,1,0,  0,0,0,0,1),nrow=5)
colnames(contrasts(Cause)) <-matrix(c("E","M","S","T"),ncol=4)
contrasts(Cause)

xdmgnd.lm6<-lm(ACCDMG~Cause,data=xdmgnd)
summary(xdmgnd.lm6)

#******************************************************
#
#  			            Session 10
#			      Multiple Linear Regression
#	              ANCOVA, PC Regression
#
#******************************************************


##************************* 
##      ANCOVA           ##
##************************* 
attach(xdmgnd) #attach the dataset to the R search path. 
detach(xdmgnd) #remove it from the search path

summary(ACCDMG) #now you can use variable names directly

## Create 2 ANCOVA models:

xdmgnd.lm1 <-lm(ACCDMG~Cause+TEMP + TRNSPD +  CARS + HEADEND1,data=xdmgnd)
summary(xdmgnd.lm1)

xdmgnd.lm2<-lm(ACCDMG~(Cause+TEMP + TRNSPD + CARS + HEADEND1)^2,data=xdmgnd)
summary(xdmgnd.lm2)

#Perform a Partial F Test: xdmgnd.lm1 vs. xdmgnd.lm2
anova(xdmgnd.lm1,xdmgnd.lm2)

## Use stepwise regression
xdmgnd.lm1.step <- step(xdmgnd.lm2)
summary(xdmgnd.lm1.step)

##************************* 
##      PC Regression    ##
##************************* 

# Source princomreg.R and PCAplots.R 
source('princompreg.R')
source('PCAplots.R')

# Create a princ comp biplot 'xdmgnd.pc' consisting of 'CARS','CARSHZD','EVACUATE','TEMP','TRNSPD','HEADEND1','HIGHSPD' ----
xdmgnd.selected<-xdmgnd[,c('CARS','CARSHZD','EVACUATE','TEMP','TRNSPD','HEADEND1','HIGHSPD')] #no response variable here.
xdmgnd.pc<-princomp(xdmgnd.selected,cor=T) 
biplot(xdmgnd.pc)

# plot loadings for xdmgnd.pc:
loadingsplot(xdmgnd.pc,k=3)

# Find out the number of PC that capture 90% of the variance 
var.comp(xdmgnd.pc,90)

# You can use the pc.reg in 'princompreg.R' to build PC regression
# Check the function to find out details.                          
xdmgnd.pcregression<-pc.reg(xdmgnd.pc,75,xdmgnd$ACCDMG) 
summary(xdmgnd.pcregression)

# Compare your models xdmgnd.pcregression 
AIC(xdmgnd.pcregression)
AIC(xdmgnd.pcregression,k=log(nrow(xdmgnd)))
AIC(xdmgnd.lm1)
