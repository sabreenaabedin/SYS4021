
# Load Data
setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")
source("AccidentInput.R")
source("SPM_Panel.R")
library(lattice)

path <- "/Users/sabreenaabedin/Desktop/class/SYS4021/TrainData"
acts <- file.inputl(path) 

###### Clean Data ######
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
totacts <- combine.data(acts, comvar)
rm(comvar) #clean up workspace

# remove the duplicates
totacts <- totacts[!(duplicated(totacts[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

#clean ACCDMG



# Categorical Variables
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))

as.numeric(totacts$TYPEQ)
#totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))

totacts$Cause <- rep(NA, nrow(totacts))
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"
totacts$Cause <- factor(totacts$Cause)

# create new variables
totacts$Casualty = totacts$TOTKLD + totacts$TOTINJ

totacts$SCALEDCAS = 10*totacts$TOTKLD + totacts$TOTINJ
totacts$SCALEDCAS

# PCAs for metrics 
totacts.pcaMetrics <- princomp(totacts[,c("ACCDMG", "TRKDMG", "TOTKLD", "EQPDMG", "TOTINJ", "SCALEDCAS")], cor = T)
biplot(totacts.pcaMetrics)
summary(totacts.pcaMetrics)
totacts.pcaMetrics$loadings


# remove NAs for quantitative variables 
totacts$ALCOHOL[is.na(totacts$ALCOHOL)] <- 0
sum(is.na(totacts$ALCOHOL))
totacts$DRUG[is.na(totacts$DRUG)] <- 0
sum(is.na(totacts$DRUG))

# PCAs for metrics + quantitative variables 
totacts.pca <- princomp(totacts[,c("TRNSPD", "Casualty", "ACCDMG", "HIGHSPD", "ALCOHOL", "DRUG", "ENGRS")], cor = T)
biplot(totacts.pca)
summary(totacts.pca)
totacts.pca$loadings

# scatterplots
uva.pairs(totacts[,c("TRNSPD", "Casualty", "ACCDMG", "HIGHSPD", "ALCOHOL", "DRUG", "ENGRS")]) 
