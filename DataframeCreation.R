
### SET YOUR OWN WORKING DIRECTORY

#setwd("C:/Users/student/sys4021/Rcode")
setwd("/Users/sabreenaabedin/Desktop/class/SYS4021") #SABREENA'S

source("AccidentInput.R")
source("SPM_Panel.R")
source("TestSet.R")
library(lattice)

### SET YOUR OWN PATH FOR DATA
#path <- "C:/Users/student/sys4021/train"
path <- "/Users/sabreenaabedin/Desktop/class/SYS4021/TrainData" #SABREENA'S


# create dataframe with common variables, clean data environment
acts <- file.inputl(path) 
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
totacts <- combine.data(acts, comvar)
rm(comvar)

# CREATE VARIABLES

totacts$Casualty <- totacts$TOTINJ + totacts$TOTKLD
totacts <- totacts[!duplicated(totacts[,c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]),]

totacts$Cause <- rep(NA, nrow(totacts))
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"
totacts$Cause <- factor(totacts$Cause)

totacts$Type <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))


# create casualty, isolate extreme (xcas) - all accidents with at least one casualty
xcasnd <- totacts[totacts$Casualty >= 1,]

#create dataframe for top 25% of accident damage
xactsndt25 <- totacts[(totacts$ACCDMG> 69914),]
quantile(totacts$ACCDMG)

#create dataframe for top 14 percent of casualties (doesn't make sense to cut of at 2.x causalties)
xcasndt14 <- xcasnd[(xcasnd$TOTINJ> 2),]
quantile(xcasnd$TOTINJ)

# sum(as.numeric(xactsndt25$ACCDMG))/sum(as.numeric(totacts$ACCDMG))
# sum(xactsndt25$ACCDMG >= 0)/sum(totacts$ACCDMG >= 0)

response.pca <- princomp(totacts[,c("Casualty", "TOTINJ", "TOTKLD", "TRKDMG", "ACCDMG", "EQPDMG")], cor=T)
biplot(response.pca)
summary(response.pca)
loadings(response.pca)

# plot(100*cumsum(as.numeric(sort(xcasnd$TOTINJ, decreasing = FALSE)))/sum(as.numeric(xcasnd$TOTINJ)), main = "73% of Injuries are caused by \n only 14% of accidents with at least one Casualty", ylab = "Percent of Cumulative Injuries", xlab = "Index sorted by increasing magnitude of TOTINJ")
# abline(v = sum(xcasnd$TOTINJ>=0)/2)
# abline(v = sum(xcasnd$TOTINJ>=0)/4)
# abline(v = sum(xcasnd$TOTINJ>=0)*.87)
# abline(h = 27.3)


# uva.pairs(totacts[,c("ACCDMG", "TOTINJ", "CARS", "TRNSPD", "ENGMIN", "BRAKEMEN")]) 
# 
# sum(xcasndt14$TOTINJ)/sum(xcasnd$TOTINJ)
# sum(xcasndt14$TOTINJ >= 0)/sum(xcasnd$TOTINJ >= 0)


