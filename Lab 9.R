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

uva.trim <- uva[-30,]
summary(uva.trim$Liver) # average = 48.69

# bootstrap the mean of liver transplants
bs.mean<-function(x,i) { return(mean(x[i])) }
bs.liv<-boot(uva.trim$Liver,bs.mean,R=1000)
bs.liv # standard error = 3.926492


# create model
r11donor <- r11donor[-30,]
r11.donor<-lm(r11donor$Liver_DD~r11donor$Liver)
summary(r11.donor)

r11.fit <- fitted(r11.donor)
r11.le <- residuals(r11.donor)
r11.mod <- model.matrix(r11.donor)

# Bootstrapping LM 
r11.liver.boot <- RTSB(r11donor$Liver_DD, r11donor$Liver, r11.fit, r11.le, r11.mod,5000)
# outcome variable, input variable, fitted, residuals, parameters, # replications





# UVA-MCV 
uva.liver<-uva$Liver[-30]
duke.liver<-duke$Liver[-30]

# Perform a paired t-test - shows significantly different
t.test(uva.liver, duke.liver,paired=T)

# Compute the difference between uva kidney transplants and mcv kidney transplants from 1988 to 2016
liv.diff<-ts(uva.liver-duke.liver,1988,2016)
ts.plot(liv.diff)

bs.mean<-function(x,i) { return(mean(x[i])) }
bs.liv<-boot(liv.diff,bs.mean,R=1000)
bs.liv # standard error = 3.869352

boot.ci(bs.liv, .95, index=1)





# uva$Liver=b0+b1*r11donor$Liver+e..
uva.liver.lm<-lm(uva$Liver[-30]~r11donor$Liver[-30])
uva.lfit <- fitted(uva.liver.lm)
uva.le <- residuals(uva.liver.lm)
uva.mod <- model.matrix(uva.liver.lm)

uva.liver.boot <- RTSB(uva$Liver[-30], r11donor$Liver[-30], uva.lfit, uva.le, uva.mod,5000)
uva.liver.boot # get statistics
uva.liver.boot$t
sqrt(abs(var(uva.liver.boot$t)))

# 95% CI 
boot.ci(uva.liver.boot, .95, index=2)


par(mfrow = c(1,2))
acf(uva.liver.lm$residuals)
pacf(uva.liver.lm$residuals)
par(mfrow = c(1,1))


Ratio <- rep(NA, 28)
for(i in 1:28){
  Ratio[i] = as.numeric(as.character(r11donor$All_Donors_DD[i]))/as.numeric(as.character(r11donor$All_Donors[i]))
}
summary(Ratio)

resid.ar <- ar(uva.liver.lm$residuals, method = "yule-walker") 
resid.ar

liver.null <- lm(uva$Liver[-30]~1)
anova(liver.null, uva.liver.lm, test = "Chi")

liver.null <- lm(uva$Liver[2:29]~1)
uvamcv.liver.lm2 <- lm(uva$Liver[2:29]~r11donor$Liver[2:29] + uva.liver.lm$residuals[1:28])
anova(liver.null, uvamcv.liver.lm2, test = "Chi")


res <- uvamcv.liver.lm2$residuals

# Perform a paired t-test - shows significantly different
t.test(res, uva$Liver[2:29], paired=T)

t.test(res, uvamcv.liver.lm2$t, paired=T)
