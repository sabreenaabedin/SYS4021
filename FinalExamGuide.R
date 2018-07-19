setwd("/Users/sabreenaabedin/Desktop/class/SYS4021") 

source("AccidentInput.R")
source("SPM_Panel.R")
source("PCAplots.R")
source("http://www.phaget4.org/R/myImagePlot.R") # heatmap
source("TestSet.R")
source("princompreg.R")
source("FactorPlots.R")
source("pc.glm.R")
source("ROC.R")
source("TSbootfunctions.R")
source("Transplant.plots.R")
library(lattice)
library(boot)
library(MASS)
library(forecast)
library(lme4) 

# CREATE DATA FRAME
path <- "/Users/sabreenaabedin/Desktop/class/SYS4021/TrainData"
data <- file.inputl(path) 
comvar <- intersect(colnames(data[[1]]), colnames(data[[8]]))
df <- combine.data(data, comvar)

df <- read.table("Spam.txt", sep = " ", header = F)

spam<-read.table('spam_ts.csv',header=T,sep=',')

attach(df) #attach the dataset to the R search path. 
detach(df) #remove it from the search path
summary(COL1) #now you can use variable names directly

dim(df)
summary(df)

######################################################
###### MULTIPLE LINEAR REGRESSION - MLR CODE #########
######################################################

# CATEGORICAL VARIABLES 
df$CATEGORYNAME <- rep(NA, nrow(df)) # create empty column
df$CATEGORYNAME[which(CONDITION)] <- "NEWINPUT" # insert new data
df$CATEGORYNAME <- factor(df$CATEGORYNAME) # factor new variable

# FACTOR BY NEW NAME
df$CATEGORY <- factor(df$NEWCATEGORYNAME, labels = c("CAT1", "CAT2"))

# QUALITATIVE VARIABLES
contrasts(COL) # check defaults
contrasts(COL)<-matrix(c(1,0,0,0,0, 0,0,1,0,0, 0,0,0,1,0,  0,0,0,0,1),nrow=5) # 5th row is all 0s
colnames(contrasts(col)) <-matrix(c("E","M","S","T"),ncol=4)

######################
# GRAPHICAL ANALYSIS #
######################

# DISTRIBUTION FUNCTION
plot(density(df$COL))

# BOX PLOT
box <-boxplot(df$COLUMN)
df.outliers <- df[df$COLUMN > box$stats[5],] # get outliers
allstats <- box$stats # boxplot statistics
df.nodups <- df.outliers[!(duplicated(df.outliers[, c("COL1", "COL2")])),] # remove duplicates

# SCATTERPLOT MATRIX - SPM
uva.pairs(df[,c("COL1", "COL2", "COL3")])
sample.df<-data.frame(df1$Col,df2$Col)
uva.pairs(as.matrix(sample.df)) # uva pairs
pairs(~ uva$Kidney[-30] + duke$Kidney[-30] + r11donor$Kidney[-30] + r11xplant$Kidney[-30]) # pairs

# PRINCIPAL COMPONENTS ANALYSIS - PCA
pred.pca <- princomp(df[,c("COL1", "COL2", "COL3")], cor = T ) # calculate principal components
biplot(pred.pca) # graph biplot

# HEATMAP
myImagePlot(table(df$CAT1, df$CAT2), title = "Title Here") # both columns must be categorical

# INTERACTION PLOTS
xyplot(RESPONSE~ COL1 | COL2 * COL3, data = df, type = c("p", "r")) # COL1 and COL2 X COL3
xyplot(RESPONSE~ COL1 | COL2 , data = df, type = c("p", "r")) # COL 1 AND COL 2

interaction.plot(df$COL1, df$COL2, df$RESPONSE)

# SPLIT QUALITATIVE VARIABLES 
CAT1.factor<-df$COL1
CAT1.factor[which(df$COL1<50)]<-'low'
CAT1.factor[which(df$COL1>=50)]<-'high'
interaction.plot(CAT1, CAT2, df$RESPONSE)  # Interaction plots with quantitative variables

#################
# LINEAR MODELS #
#################

# BUILD LINEAR REGRESSION MODELS - LM
df.lm<-lm(RESPONSE~ COL1 + COL2 + COL3, data=df) # can use . to use all variables
summary(xdmgnd.lm1) # summary and p-value
names(xdmgnd.lm1)
coef(xdmgnd.lm1)
sum(xdmgnd.lm1$res^2) # sum of r^2

#  INTERACTIONS
df.lm<-lm(RESPONSE~(COL1+COL2)^2,data=df)

#  SECOND ORDER                 
df.lm<-lm(RESPONSE~(COL1 + COL2)^2+I(COL1^2)+I(COL2^2),data=df)

# STEPWISE REGRESSION
df.lm.step<-step(df.lm) # start with a lm
summary(df.lm.step)

###############
# ASSESSMENTS #
###############

# CRITERION BASED ASSESSMENTS 
summary(df.lm)$adj.r.squared # adj r^2
AIC(df.lm) # AIC
AIC(df.lm,k=log(nrow(df))) # BIC

# PARTIAL F TEST
anova(df.lm,df.lm.step) # only for nested models

# TEST SETS
df.data<-test.set(df, 1/3) # subset data
lm.train<-lm(RESPONSE~COL1+COL2,data=df.data$train) # build train model - use training data
lm.pred<-predict(lm.train,newdata=df.data$test) # predict - use testing data
pmse.lm<-mse(lm.pred,df.data$test$RESPONSE) # compute PMSE
pmse.lm

# compare over many runs
pmse1.result<-NULL; #Two empty vectors to record PMSEs
pmse2.result<-NULL;

for (i in c(1:20)){
  df.data<-test.set(df, 1/3)
  
  lm1.train<-lm(RESPONSE~COL1+COL2,data=df.data$train) # build train model
  lm2.train<-lm(RESPONSE~COL1+COL2+COL3,data=df.data$train)
  
  lm1.pred<-predict(lm1.train,newdata=df.data$test) # predict
  lm2.pred<-predict(lm2.train,newdata=df.data$test) 
  
  pmse.lm1<-mse(lm1.pred,df.data$test$RESPONSE) # compute PMSE
  pmse.lm2<-mse(lm2.pred,df.data$test$RESPONSE)
  pmse1.result<-c(pmse1.result,pmse.lm1)
  pmse2.result<-c(pmse2.result,pmse.lm2)
}

# Compare models based over 20 runs of PMSE
plot(pmse1.result,type='b',col='blue',xlab="Index", ylab="PMSE")
lines(pmse2.result,type='b',col='red')
title(main="Title")

# PAIRED T-TEST
t.test(pmse1.result,pmse2.result,paired=T)

# WILCOXON TEST
wilcox.test(pmse1.result,pmse2.result,paired=T)

# CROSS VALIDATION
lm1.err<-cv.glm(df,df.glm,K=10) # (dataframe, GENERALIZED linear model, #folds)
lm1.err$delta
lm2.err<-cv.glm(df,df.glm,K=10)
lm2.err$delta # first is raw cross validation estimate of prediction error, second is adjusted estimate

# DIAGNOSTICS PLOT
par(mfrow=c(2,2))
plot(df.lm1, labels.id = NULL)
par(mfrow=c(1,1)) 

plot(df.lm1,which=1) #Residual vs. Fitted
plot(df.lm1,which=2) #QQ
plot(df.lm1,which=3) #Scale-Location
plot(df.lm1,labels.id = NULL, which=4) #Cook's distance
plot(df.lm1,which=5) #Redisuals vs. Leverage
plot(df.lm1,which=6) #Cook's dist vs. Leverage

plot(density(df$RESPONSE)) # plot response variable

# BOX-COX PLOT - transformation
boxcox(df.lm) # box-cox plot
boxcox(df.lm,plotit=F) # just values

max(boxcox(df.lm, plotit = F)$y) # find y with max labmda
boxcox(df.lm, plotit = F)$x[which.max(boxcox(df.lm, plotit = F)$y)] # best labmda
L<-boxcox(df.lm, plotit = F)$x[which.max(boxcox(df.lm, plotit = F)$y)] # store as L

df.lm.boxcox<-lm(RESPONSE^L ~COL1+COL2,data=df) # use L in model
summary(df.lm.boxcox)
plot(density(df$RESPONSE^L))

# ANCOVA
lm1 <-lm(RESPONSE~ CAT + COL1 + COL2, data=df) # create two models containing qualitative variables
lm2 <-lm(RESPONSE~ CAT + COL1, data=df)
anova(lm1,lm2) # partial f test

lm.step <- step(lm2) # use stepwise regression

# PC REGRESSION
df.selected<-df[,c('COL1', 'COL2', 'COL3')] #no response variable here.
df.pc<-princomp(df.selected,cor=T) # get principal components
biplot(df.pc)

loadingsplot(df.pc,k=3) # plot loadings
var.comp(xdmgnd.pc,90) # num of PC that capture 90% of the variance 

df.pcregression<-pc.reg(df.pc,75,df$RESPONSE) # (pc object, goal %, response variable)
summary(df.pcregression)
AIC(df.pcregression) # Compare your models
AIC(df.pcregression,k=log(nrow(DF)))
AIC(df.lm)

################################################
###### GENERALIZED LINEAR MODELS - GLM #########
################################################

### SPAM SPECIFIC LAB QUESTIONS 

sum(spam[,58])/length(spam[,58]) # What proportion is spam? - with binary variable

uva.pairs(spam[,c(1:10,58)]) # compares only specific variables

# Obtain boxplots with variables 1-9 vs. the response.
par(mfrow = c(3,3))
for(i in 1:9) {
  boxplot(spam[,i]~spam[,58], xlab = "Type of Email", ylab = "Count", main = paste("Variable", i))  
}
par(mfrow = c(1,1))

########################
# PRINCIPAL COMPONENTS #
########################

df.pca = princomp(df[,1:57], cor = T) # numbers are variable range
biplot(spam.pca)

summary(df[,which(df.pca$loadings[,2] > 0.2)]) # find outliers

biplot.fact(df.pca, spam[,58]) # Obtain the biplot.fact of your principal components.
legend(20, 15, legend = c("Spam", "Ham"), pch = c(18, 19), col = c("red", "blue")) # 2 color

#####################
# LOG OF PREDICTORS #
#####################
Lspam <- log(spam[,-58] + .1) # log transform of the predictor variables
Lspam[,58] <- spam[,58] # keep response variable the same

###################################
# GENERALIZED LINEAR MODELS - GLM #
###################################

# CREATE GLM
df.glm.main <- glm(response~., data = df, family = binomial)
summary(df.glm.main)

# PREDICT ??????
predict(df.glm.main, newdata = spam[1,]) #????????????????????
exp(predict(spam.glm.main, newdata = spam[1,]))
exp(predict(spam.glm.main, newdata = data.frame(spam[1,c(1:50, 52:57)], V51 = 1)))

# MODEL UTILITY TEST FOR GLM - MAXIMUM LIKELIHOOD TEST 
df.null <- glm(response~1, data = df, family = binomial)
anova(df.null, df.glm.main, test = "Chi") # using Chi2 statistic

# T-TEST ON SINGLE COEFFICIENT FOR GLM - PARTIAL F TEST ?? should use drop1()
df.57 <- update(df.glm.main, .~.-V57, data = df) # same glm but without VAR 57
anova(df.no57, df.glm.main, test = "Chi") # compare to see if model without 57 is significantly worse

(exp(spam.glm.main$coefficients[52])-1)*100 # contribution of variable 51 (52 = 51 + 1 intercept) ???

# DROP1 - SEQUENTIALLY DROP TERMS (functional equivalent to t-test for MLR)
drop1(df.glm.main, response~., test = "Chi", data = df)

################
# INTERACTIONS #
################

df.glm <- glm(response~., data = df, family = binomial) # main effects
df.glm2 <- glm(response~. + (V5+V6+V7)^2, data = df, family = binomial) # interaction
anova(spam.glm, spam.glm2, test = "Chi") # partial likelihood test

Ldf <- log(df[,-58] +.1) # log transforemd predictors
Ldf$V58 <- df[,58]

Ldf.glm <- glm(response~., data = Ldf, family = binomial) # main effects
Ldf.glm2 <- glm(response~. + (V5+V6+V7)^2, data = Ldf, family = binomial) # interaction terms
anova(Lspam.glm, Lspam.glm2, test = "Chi") #partial likelihood test

# AIC & BIC
AIC(df.glm)
BIC(df.glm)

###################################
# PRINCIPAL COMPONENTS REGRESSION #
###################################

df.pca <- princomp(df[,-58], cor = T) # get pcs

screeplot(df.pca) # scree plot - variances accounted for by components

var.comp(df.pca, 90) # how many pcs to account for 90% of the variance 

pca.glm98 <- pc.glm(df.pca, 98, df[,58]) # get the principal component regression results

# MODEL UTILITY TEST
spampc.null <- pc.null(spam.pca, 98, spam[,58])
anova(spampc.null, spampca.glm98, test = "Chi")

# PARTIAL LIKELIHOOD TEST
anova(spampca.glm50, spampca.glm98, test = "Chi") # need to create second model

AIC(spampca.glm98)
BIC(spampca.glm98)

######################
# EVALUATION OF GLMS #
######################

# TESTING & TRAINING SETS
test.df <- test.set(df, .33) # Obtain test and training sets

df.glm <- glm(response~., data = test.df$train, family = binomial) # test data model
resid(spam.glm) # residuals
summary(spam.glm)

# MODEL UTILITY TEST
df.null <- glm(response~1, data = df.test$train, family = binomial)
anova(df.null, df.glm, test = "Chi")

# LOG TRANSFORM GLM USING TRAINING DATA 
Ldf.train <- log(df.test$train[,-58] + .1)
Ldf.train$response <- df.test$train$response
Ldf.glm <- glm(response~., Ldf.train, family = binomial)
summary(Ldf.glm)

# DIAGNOSTIC PLOTS
par(mfrow = c(2,2))
plot(df.glm)
par(mfrow = c(1,1))

which(hatvalues(df.glm) > 0.8) # determine which points are influential

# STEPWISE REGRESSION
DF.step <- step(df.glm, trace = 0) # main effects stepwise
length(df.step$coeff) - 1 # number of variables

df.step1 <- step(df.glm, steps = 5, trace = 0) # use only 5 steps

# INTERACTION PLOT
V1.factor<-df$V1
V1.factor[which(df$V1<=median(df$V1))]<-'low'
V1.factor[which(df$V1>median(df$V1))]<-'high'

with(spam, interaction.plot(x.factor=V38.factor, trace.factor=V1.factor, response=V58, type="b",ylab="Response variable", main="Interaction Plot",pch=c(1,19)))

# PRINCIPAL COMPONENTS REGRESSION
df.pca <- princomp(df.test$train[,-58], cor = T) # accounting for 90% of variance
pca.glm90 <- pc.glm(df.pca, 90, df.test$train[,58])

# PREDICTIONS WITH TEST DATA
df.pred <- predict(df.glm, type = "response", newdata = df.test$test) # get predictions - glm was made with training data
pca.pred <- predict.pc.glm(pca.glm98, df.pca, df.test$test[,1:57] ) # get predictions from pc glm (glm, pc object, data set)

# CONFUSION MATRICES / TABLES
score.table(df.pred, df.test$test[,58], .5) # decision threshold = 0.5

# ROC CURVE
plot.roc(df.pred, df.test$test[,58], main = "ROC Curve", col = "blue")
lines.roc(df2.pred, df.test$test[,58], col = "orange")
legend(.6, .57, legend = c("Model1", "Model2"), col = c("blue", "orange", "green", "red", "cyan"), lwd = 1)

########################################
###### TIME SERIES MODELS - TM #########
########################################

df.ts<-ts(df$count) # create a time series
df.ts<-ts(df$count[1:464]) # create a constrained time series

# TREND AND SEASONALITY
time.df<-c(1:(length(df.ts)-7)) # create a time variable

df.trend<-lm(df.ts[time.df]~time.df) # TREND: lm of response over time
summary(df.trend)

plot(df.ts[time.df], type = "l") # plot general trend over time
abline(spam.trend, col = "red") # add trendline

# PERIODOGRAM
pg.df<-spec.pgram(df.ts,spans=9,demean=T,log='no')
max.omega.ham<-pg.ham$freq[which(pg.ham$spec==max(pg.ham$spec))] # Find the peak
1/max.omega.ham # Period
df.ts2 = ts.intersect(df.ts, newdata=lag(df.ts,-7), dframe=TRUE) # can build new model based on lag (period)

# Build a new model, ham.season which predicts ham.ts with a 7 day lag (based on period)
ham.ts.7 = ts.intersect(ham.ts, ham7=lag(ham.ts,-7), dframe=TRUE)
ham.season <- lm(ham.ts~ ham7, data=ham.ts.7)

summary(ham.season) # model is significant


# AUTOREGRESSIVE (AR), MOVING AVERAGE (MA) & ARIMA Models  
e.ts.df<-ts(df.trend$residuals) # get residuals
plot(e.ts.df, ylab = "Residuals")

# PLOT ACF AND PACF
par(mfrow=c(1,2))
acf(e.ts.spam, main="ACF of Residuals from spam.trend")
pacf(e.ts.spam,main="PACF of Residuals from spam.trend")
par(mfrow=c(1,1))

# DIFFERENCE ACF AND PACF ????
par(mfrow=c(1,2))
acf(diff(e.ts.spam), main="Diff ACF of Residuals from spam.trend")
pacf(diff(e.ts.spam),main="Diff PACF of Residuals from spam.trend")
par(mfrow=c(1,1))

# order = (p, d, q) 
df.ar <- arima(e.ts.df, order=c(1,0,0)) # ar(1) p=1
df.arma <- arima(e.ts.df, order=c(1,0,1)) # arma(1,1) p=1, q=1 
df.arima <- arima(e.ts.df, order=c(1,1,1)) # arima(1,1,1) p=1, d=1, q=1

# AUTO ARIMA
df.auto <- auto.arima(e.ts.df, trace=TRUE)

# CRITERION
summary(df.auto)
AIC(df.auto)
AIC(df.auto, k = log(length(e.ts.df))) # BIC

# TIME SERIES DIAGNOSTICS - TS
tsdiag(DF.arma,gof.lag=20)

# FORECASTING
df.forecast<-forecast(df.auto,h=7) # forecast next 7 days (model, # forecasts)
plot(df.forecast)

# TEST PREDICTION PERFORMANCE ????
next.week.time<-c((length(spam.ts)-6):(length(spam.ts))) # the next week or the test period in days
next.week<-data.frame(time.spam = next.week.time, count = spam.ts[next.week.time]) # the test data frame
next.week.ts <- spam.ts[next.week.time] # the actual time series for the test period
next.week.ts<-ts(next.week$count)

# PREDICTION
E_Y.pred<-predict(spam.trend,newdata=next.week)
e_t.pred<-forecast(spam.auto,h=7)
next.week.prediction<-E_Y.pred+e_t.pred$mean

# MSE
mean((next.week.prediction-next.week$count)^2)

# PLOT REAL VS PREDICTED
plot(ts(next.week$count),type='o')
lines(ts(next.week.prediction),col='red',type='o')
lines(1:7, E_Y.pred + e_t.pred$lower[,2], col = "red", lty = "dashed")
lines(1:7, E_Y.pred + e_t.pred$upper[,2], col = "red", lty = "dashed")
legend(4,12, legend = c("Actual", "Predicted"), lwd = 2, col = c("black", "red")) 


##############################################
###### BOOTSTRAPPING AND TRANSPLANTS #########
##############################################

# TRANSPLANT PLOTS
donortype.plot(cbind(r11xplant$Lung_DD[-30], r11xplant$Lung_LD[-30], r11donor$Lung_DD[-30],r11donor$Lung_LD[-30]), title = "Lung")
region.plot(cbind(r11xplant$Heart[-30], r11donor$Heart[-30], uva$Heart[-30], unc$Heart[-30], mcv$Heart[-30], duke$Heart[-30]), title = "Heart")
center.plot(cbind( uva$Pancreas[-30], unc$Pancreas[-30], mcv$Pancreas[-30], duke$Pancreas[-30]), title = "Pancreas") # gives temporal data

#########################
# BOOTSTRAP DIFFERENCES #
#########################

# DIFFERENCES
uva.kidney<-uva$Kidney
mcv.kidney<-mcv$Kidney
kid.diff<-ts(uva.kidney-mcv.kidney,1988,2016) # compute differences as ts
ts.plot(kid.diff,ylab='UVa-MCV',main = "Difference in Number of Transplants, UVA-MCV") # plot time series
t.test(uva.kidney, mcv.kidney,paired=T) # Perform a paired t-test

# BOOTSTRAP
bs.mean<-function(x,i) { return(mean(x[i])) } # mean was chosen statistic
bs.kid.diff<-boot(kid.diff,bs.mean,R=2000) # Bootstrap mean differences - boot(data= , statistic= , R= ), R = # replications
bs.kid.diff # original = t-test, bias = difference between mean of all samples and orginial estimate, std error = standard deviation of all boostrap samples
plot(bs.kid.diff,index=1) 

# CONFIDENCE INTERVALS
boot.ci(bs.kid.diff,0.95,type=c('bca','perc')) 

##########################
# BOOTSTRAP LINEAR MODEL #
##########################

# Build a linear model, uva.kid.lm that predicts uva kidney transplants by region 11 kidney donors from 1988-2016
uva.kid.lm <- lm(uva$Kidney[-30]~r11donor$Kidney[-30])
summary(uva.kid.lm) # significant

uva.kfit <- fitted(uva.kid.lm) # fitted
uva.ke <- residuals(uva.kid.lm) # residuals
uva.mod <- model.matrix(uva.kid.lm) # regression parameters

uva.kid.boot <- RTSB(uva$Kidney[-30], r11donor$Kidney[-30], uva.kfit, uva.ke, uva.mod,2000) # boostrap coefficients for distribution
uva.kid.boot

boot.ci(uva.kid.boot, .99) # Get the 99% CI for uva.kid.boot
plot(uva.kid.boot, index = 2) # Plot the results for the coeffiecients - bootstrap

#	A set of configurable plots
par(mfrow = c(1,2))
hist(uva.kid.boot$t[,2], main = "Region 11 Donors",xlab ="Coefficient Values",   col = "steelblue", breaks = 50)
qqnorm(uva.kid.boot$t[,2])
qqline(uva.kid.boot$t[,2])
par(mfrow = c(1,1))

#########################
# BOOTSTRAP TIME SERIES #
#########################

diff.ar.kid <- ar(uva.kid.lm$residuals, method = "yule-walker") #	Fit an ar model to the residuals
diff.ar.kid # order = number of autoregressive terms needed ???

plot(bs.kid.diff,index=1)  # plot bootstrap time series

# If we use diff.ar.kid ????????
uva.kid.lm2<- lm(uva$Kidney[2:29]~r11donor$Kidney[2:29] + uva.kid.lm$residuals[1:28])
summary(uva.kid.lm2)

# The problem here is we have only a few observations (28)
par(mfrow=c(2,2))
plot(uva.kid.lm2)
par(mfrow=c(1,1))

##########################################
###### MULTILEVEL MODELING - MLM #########
##########################################

##################
# COMBINING DATA #
##################

# FOR TRANSPLANT DATA
currentYear <- 2016 
nYear <- length(uva$Year) # Years
comvar <- intersect((intersect(names(uva), names(mcv))),(intersect(names(uva), names(unc))))
comvar <- comvar[1:7] # keep selected columns
xplant <- rbind(uva[,comvar], mcv[,comvar], duke[,comvar], unc[,comvar])

SchoolNames <- c("UVA", "MCV", "Duke", "UNC") # create school variable
xplant$School <- c(rep("UVA", 30), rep("MCV", 30), rep("Duke", 30), rep("UNC", 30))
xplant$School <- factor(xplant$School)

xplant$nYears <- xplant$Year - 1987
xplantC <- xplant[-c(seq(30,120,30)),] # So, remove the partial year
summary(xplantC)
str(xplantC) # get overview and type of data

# CATEGORICAL BOX PLOTS
bwplot(Kidney~School, data = xplantC)

#############################
# VARY INTERCEPTS - 0 SLOPE #
#############################

# CREATE MODEL
xplant.lme1 <- lmer(Kidney~ 1 + (1|School) , data = xplantC, REML = F)
# predict kidney as a proportional model + (intercept | varies by school)
# MAXIMUM LIKELIHOOD: REML = F (more accurate, slower)

coef(xplant.lme1) # coefficient = fixed + random
fixef(xplant.lme1) # fixed part of intercept
ranef(xplant.lme1) # random part of intercept
dotplot(ranef(xplant.lme1,  condVar = T)) # dot plot of random effects

# PLOT INTERCEPTS
center.plot(cbind( uva$Kidney[-nYear], unc$Kidney[-nYear], mcv$Kidney[-nYear], duke$Kidney[-nYear]), Year = seq(1988,(1988+nYear - 2)), title = "Kidney")

sapply(1:4, function(i){ # add averages onto plot
  abline(h = coef(xplant.lme1)$School[i,1])
  text(2016, coef(xplant.lme1)$School[i,1], levels(xplantC$School)[i])
})

# DIAGNOSTICS - MLM
plot(xplant.lme1) # residuals vs. fits 
qqnorm(residuals(xplant.lme1))
qqline(residuals(xplant.lme1)) 

# SERIAL CORRELATION
sapply(1:4, function(x){
  acf(residuals(xplant.lme1)[seq(1,29)+((x-1)*29)], main = SchoolNames[x])
})
sapply(1:4, function(x){
  pacf(residuals(xplant.lme1)[seq(1,29)+((x-1)*29)], main = SchoolNames[x])
})


####################################
# VARY INTERCEPTS - CONSTANT SLOPE #
####################################

xplant.lme2 <- lmer(Kidney~ nYears + (1|School), data = xplantC, REML = F)
#  predict kidney as a function of years with intercept 1 varying by school

coef(xplant.lme2)
fixef(xplant.lme2)
ranef(xplant.lme2)
dotplot(ranef(xplant.lme2,  condVar = T))

# PLOT INTERCEPTS
center.plot(cbind( uva$Kidney[-nYear], unc$Kidney[-nYear], mcv$Kidney[-nYear], duke$Kidney[-nYear]), Year = seq(1988,(1988+nYear - 2)), title = "Kidney")
sapply(1:4, function(i){
  abline(a = coef(xplant.lme2)$School[i,1] - 1988*coef(xplant.lme2)$School[i,2], b = coef(xplant.lme2)$School[i,2], col = c( "blue3" , "purple" , "lightblue3","orange")[i])
})
abline(a = 67.04913 - 1988*2.72598, b = 2.72598, col = "orange") # simpler trend line formula

# DIAGNOSTICS
plot(xplant.lme2) # residuals vs. fits
qqnorm(residuals(xplant.lme2))
qqline(residuals(xplant.lme2)) 
sapply(1:4, function(x){
  acf(residuals(xplant.lme2)[seq(1,29)+((x-1)*29)], main = SchoolNames[x])
})
sapply(1:4, function(x){
  pacf(residuals(xplant.lme2)[seq(1,29)+((x-1)*29)], main = SchoolNames[x])
})


# COMPARE MLM USING ANOVA
anova(model1, model2) # notice no test = "Chi"

######################################
# VARYING INTERCEPTS & VARYING SLOPE #
######################################

# XY PLOTS - VIEW VARYING SLOPES
xyplot(Kidney~nYears|School, data = xplantC,type = c("p", "r"))

# CREATE MODEL
xplant.lme3 <- lmer(Kidney~ nYears + (nYears|School), data = xplantC, REML = T)
# predict Kidney as a function of number of years, and the intercept, based on number of years, varies by school

coef(xplant.lme3)
fixef(xplant.lme3)
ranef(xplant.lme3)
dotplot(ranef(xplant.lme3,  condVar = T), scales = list(relation = "free"))

# DIAGNOSTICS
plot(xplant.lme3) # residuals vs. fits
qqnorm(residuals(xplant.lme3))
qqline(residuals(xplant.lme3))

# PLOT VARYING INTERCEPTS AND VARYING SLOPE
center.plot(cbind( uva$Kidney[-nYear], unc$Kidney[-nYear], mcv$Kidney[-nYear], duke$Kidney[-nYear]), Year = seq(1988,(1988+nYear - 2)), title = "Kidney")
sapply(1:4, function(i){
  abline(a = coef(xplant.lme3)$School[i,1] - 1988*coef(xplant.lme3)$School[i,2], b = coef(xplant.lme3)$School[i,2], col = c( "blue3" , "purple" , "lightblue3","orange")[i])
})

# SERIAL CORRELATION
sapply(1:4, function(x){
  acf(residuals(xplant.lme3)[seq(1,29)+((x-1)*29)], main = SchoolNames[x])
})
sapply(1:4, function(x){
  pacf(residuals(xplant.lme3)[seq(1,29)+((x-1)*29)], main = SchoolNames[x])
})

###############################################
# VARYING INTERCEPTS & VARYING SLOPE  & AR(1) #
###############################################

# Need a new DF with lag 1
xplantL1 <- data.frame(xplantC[-seq(1,4*29, by =29),], KidneyL1 = xplantC$Kidney[-seq(29, 4*29, by = 29)])
head(xplantL1[,c("Kidney", "KidneyL1")])

# Model with lag 1
xplant.lme5 <- lmer(Kidney~ nYears + KidneyL1+ (nYears|School), data = xplantL1, REML = F)

coef(xplant.lme5) 
fixef(xplant.lme5)
ranef(xplant.lme5)
dotplot(ranef(xplant.lme5,  condVar = T), scales = list(relation = "free"))

# DIAGNOSTICS
plot(xplant.lme5) # residuals vs. fits
qqnorm(residuals(xplant.lme5))
qqline(residuals(xplant.lme5))

sapply(1:4, function(x){
  acf(residuals(xplant.lme5)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})
sapply(1:4, function(x){
  pacf(residuals(xplant.lme5)[seq(1,28)+((x-1)*28)], main = SchoolNames[x])
})

# COMPARE MODELS - TWO MODELS MUST USE SAME DF
anova(xplant.lme3, xplant.lme5) #notice no test = "Chi" 

##############
# PREDICTION #
##############

newkdata <- data.frame(Kidney =0, nYears = 30, KidneyL1 = xplantC$Kidney[29], School = "UVA")

#get model parameters
mm <- model.matrix(terms(xplant.lme5), newkdata)

#compute the prediction use parameters for UVA
xplant.lme5.pred <- mm %*% t(coef(xplant.lme5)[[1]][which(row.names(ranef(xplant.lme5)$School) == newkdata$School),])
xplant.lme5.pred

# PLOT PREDICTION
plot(xplantC$Year[1:29], xplantC$Kidney[1:29], type = "b", pch = 19, xlim = c(1988, 2017), ylim = c(0, 225), ylab = "Number of Transplants", xlab = "Year", main = "Kidney Transplants at UVA")

points(2017,quantile(xplant.lme5.pred, c(.5)) , col = "red", pch = 19) # lme prediction
points(2017, xplant$Kidney[30], col = "blue", pch = 19) # Current
points(2017, 2*xplant$Kidney[30], col = "cyan", pch = 19) # Twice current

legend(1990, 100, legend = c("Current", "2 X Current", "LME Prediction"), col = c("blue", "cyan", "red", "green"), pch = 19)
