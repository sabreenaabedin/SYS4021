#********************************************************************************
#					Univariate Graphics
#********************************************************************************

# Set working directory
setwd("/Users/sabreenaabedin/Desktop/class/SYS4021/TrainData")
getwd()
dir()

# Read in the accident files one at at time
acts16 <- read.csv("RailAccidents16.csv")
summary(acts16)
summary(acts16[,c("ACCDMG", "TOTKLD", "CARS")])
mean(acts16$TOTKLD)
var(acts16$TOTKLD)
round(mean(acts16$TOTKLD))

setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")
source("AccidentInput.R")

path <- "/Users/sabreenaabedin/Desktop/class/SYS4021/TrainData"
acts <- file.inputl(path) 


#################
#	Data Cleaning
#################

# Notice that the number of columns changes from year to year
ncol(acts[[1]])
ncol(acts[[8]])

# Get a common set the variables - since different years have different variables
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))
comvar
	
# Now combine the data frames for all 14 years
totacts <- combine.data(acts, comvar)
rm(comvar) #clean up workspace
dim(totacts)
totacts


#***********************************
# 	histograms of ACCDMG
#***********************************

hist(acts[[11]]$ACCDMG) # for 2011
hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

# Different bin widths
hist(acts[[11]]$ACCDMG, breaks = "scott", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[11]]$ACCDMG, breaks = "fd", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[11]]$ACCDMG, breaks = 20, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[11]]$ACCDMG, breaks = 100, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

# other years
nclass.Sturges(totacts)
hist(totacts$ACCDMG)

par(mfrow = c(2,2))
hist(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[4]]$ACCDMG, main = "Total Accident Damage in 2004", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[8]]$ACCDMG, main = "Total Accident Damage in 2008", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")
par(mfrow = c(1,1))

# 4x3 panels of histograms from each year 
par(mfrow = c(4,3))
for(i in 1:11)
{
	j <- which(colnames(acts[[11]]) == "ACCDMG")
	if(i < 10)
	{
	hist(acts[[i]][,j], main = paste("Total Accident Damage in 200", i, sep = "") , xlab = "Dollars ($)", col = "steelblue")	
	}
	else
	{
	hist(acts[[i]][,j], main = paste("Total Accident Damage in 20", i, sep = "") , xlab = "Dollars ($)", col = "steelblue")	
	}
	
}
par(mfrow = c(1,1))

# making numeric 
asnumeric(acts16["TOTKLD"])
totalkilled <- sapply(acts16["TOTKLD"], as.numeric)
totalkilled
hist(totalkilled)


# Damage in constant scales
par(mfrow = c(4,3))
for(i in 1:11)
{
	j <- which(colnames(acts[[11]]) == "ACCDMG")
	if(i < 10)
	{
	hist(acts[[i]][,j], main = paste("Total Accident Damage in 200", i, sep = "") , xlab = "Dollars ($)", col = "steelblue", xlim = c(0,1.7e7), ylim = c(0,4000))	
	}
	else
	hist(acts[[i]][,j], main = paste("Total Accident Damage in 20", i, sep = "") , xlab = "Dollars ($)", col = "steelblue", xlim = c(0,1.7e7), ylim = c(0, 4000))	
	}
par(mfrow = c(1,1))

#************************
# 				Boxplots 
#************************

boxplot(acts[[11]]$ACCDMG)
boxplot(acts16[c("TRNSPD")])

boxplot(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", ylab = "Dollars ($)", col = "steelblue", pch = "*")
boxplot(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", ylab = "Dollars ($)", col = "steelblue", pch = 4)

boxplot(acts[[6]]$ACCDMG, main = "Total Accident Damage in 2006", ylab = "Dollars ($)", col = "steelblue")
bwplot(totacts$EQPDMG)

par(mfrow = c(2,2))
boxplot(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", ylab = "Dollars ($)", col = "steelblue")
boxplot(acts[[4]]$ACCDMG, main = "Total Accident Damage in 2004", ylab = "Dollars ($)", col = "steelblue")
boxplot(acts[[8]]$ACCDMG, main = "Total Accident Damage in 2008", ylab = "Dollars ($)", col = "steelblue")
boxplot(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", ylab = "Dollars ($)", col = "steelblue")
par(mfrow = c(1,1))

boxplot(acts16["HIGHSPD"])
summary(acts16["ACCDMG"])
sum(acts16["TOTKLD"])

par(mfrow = c(2,2))
boxplot(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7))
boxplot(acts[[4]]$ACCDMG, main = "Total Accident Damage in 2004", ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7))
boxplot(acts[[8]]$ACCDMG, main = "Total Accident Damage in 2008", ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7))
boxplot(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7))
par(mfrow = c(1,1))

par(mfrow = c(4,3))
for(i in 1:11)
{
	j <- which(colnames(acts[[11]]) == "ACCDMG")
	if(i < 10)
	{
	boxplot(acts[[i]][,j], main = paste("Total Accident Damage in 200", i, sep = "") , ylab = "Dollars ($)", col = "steelblue")	
	}
	else
	{
	boxplot(acts[[i]][,j], main = paste("Total Accident Damage in 20", i, sep = "") , ylab = "Dollars ($)", col = "steelblue")	
	}
	
}
par(mfrow = c(1,1))

par(mfrow = c(4,3))
for(i in 1:11)
{
	j <- which(colnames(acts[[11]]) == "ACCDMG")
	if(i < 10)
	{
	boxplot(acts[[i]][,j], main = paste("Total Accident Damage in 200", i, sep = "") , ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7), ylim = c(0,4000))	
	}
	else
	boxplot(acts[[i]][,j], main = paste("Total Accident Damage in 20", i, sep = "") , ylab = "Dollars ($)", col = "steelblue", ylim = c(0,1.7e7), ylim = c(0, 4000))	
	}
par(mfrow = c(1,1))

hist(totacts$TEMP)

#*************
#		QQ Plots
#*************

qqnorm(acts[[11]]$ACCDMG, main = "Total Accident Damage")
qqline(acts[[11]]$ACCDMG)

qqnorm(acts[[11]]$TEMP, main = "Accident Temperature")
qqline(acts[[11]]$TEMP)

#******************************************************
#					Session 3
#			 		Multivariate Displays
#******************************************************


# Variable names
names(totacts)

# View the data types
str(totacts)

# Look at the type for TYPE using summary
totacts$type <- factor(totacts$TYPE)
summary(totacts$type)
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))

# Use table() to see the frequencies
table(totacts$TYPE)

# Use barplot() to graph this
barplot(table(totacts$TYPE))

# CATEGORICAL DATA

# factor type - categorical data
str(totacts$TYPEQ)
as.numeric(totacts$TYPEQ)
totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))
summary(totacts$TYPEQ)

#find the index where the first substring is a character, replace entire cause variable
summary(totacts$CAUSE)

totacts$Cause <- rep(NA, nrow(totacts))
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"
totacts$Cause <- factor(totacts$Cause)

table(totacts$Cause)
barplot(table(totacts$Cause))

# Now do the summary for totacts, but remove all narratives and any other variables you won't use for this project
cbind(1:ncol(totacts), names(totacts))

#**************************************************
#			Scatter Plot Matrices
#**************************************************

# Scatter plots
plot(2001:2016, tapply(totacts$ACCDMG, as.factor(totacts$YEAR), sum), type = "l", ylab = "Damage ($)", xlab = "Year", main = "Total Damage per Year")

setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")
source("SPM_Panel.R")

# without panel functions for 2010
pairs(~  TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD , data = acts[[10]])

# with panel function (correlation coefficient)
uva.pairs(acts[[10]][,c("WEATHER", "TRNSPD", "COUNTY", "HIGHSPD", "TOTKLD", "TOTINJ", "ACCDMG")]) 
uva.pairs(totacts[,c("TRKDMG", "EQPDMG","TOTKLD", "TOTINJ", "ACCDMG")]) 

# Print as png to avoid problems in the document
png("metrics.png")
uva.pairs(acts[[10]][,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")]) 
dev.off()


#**************************************************
#		Trellis Categorical Plots
#**************************************************

library(lattice)

# Plotting damage per year
bwplot(as.factor(YEAR)~ACCDMG, data = totacts, main = "Box Plots of Accident Damage", xlab = "Damage ($)", ylab = "Year")
boxplot(totacts$ACCDMG)
png("bw.png")

# Find out the worst accident for damage
which(totacts$ACCDMG == max(totacts$ACCDMG))
totacts[50976,"ACCDMG"]

summary(totacts$TOTINJ)
sum(totacts$TOTKLD >= 1)

# Plotting accident cause vs. damage
bwplot(Cause~ ACCDMG, main = "Box Plots of Log(Accident Damage)", xlab = "Damage ($)", ylab = "Accident Cause", data = totacts)
bwplot(Cause~ log(ACCDMG+1), main = "Box Plots of Log(Accident Damage)", xlab = "log(Damage ($))", ylab = "Accident Cause", data = totacts)

# Plot cause vs. no. killed or injured
bwplot(Cause ~ TOTKLD, main = "Box Plots of Total Killed", xlab = "Total Killed", ylab = "Accident Cause", data = totacts)
bwplot(Cause~ TOTINJ, main = "Box Plots of Total Injured", xlab = "Total Injured", ylab = "Accident Cause", data = totacts)

# X-Y plots conditioned on cause
xyplot(ACCDMG~TOTKLD | Cause, main = "Damage vs. Killed Conditioned on Cause", xlab = "Total Killed", ylab = "Total Accident Damage", data = totacts)


#******************************************************
#					Session 4
#				Duplicates, 
#				Categorial Variable Relationships &
#				Extreme values
#******************************************************

#************************
#			Extreme Points
#************************

hist(totacts$ACCDMG)

# Get the values in the box plot
dmgbox <- boxplot(totacts$ACCDMG)
length(dmgbox$out)
dmgbox$stats

# What proportion of accidents are extreme?
length(dmgbox$out)/nrow(totacts)

# Proportion of costs
sum(as.numeric(totacts$ACCDMG[which(totacts$ACCDMG > dmgbox$stats[5])]))/sum(as.numeric(totacts$ACCDMG))
dmgbox$stats

# Create a data frame with just the extreme ACCDMG accidents
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]
dim(xdmg)

# Look at the graphs of these extreme accidents
hist(xdmg$ACCDMG)
boxplot(xdmg$ACCDMG, col = "steelblue", main = "Accidents with Extreme Damage", ylab = "Cost ($)")

plot(1:16, tapply(xdmg$ACCDMG, xdmg$YEAR, sum), type = "l", xlab = "Year", ylab = "Total Damage ($)", main = "Total Accident Damage per Year")

# Frequency of accident types
barplot(table(xdmg$TYPE)) #compare with the totacts plot

# SPM of metrics & train variables
uva.pairs(xdmg[,c("ACCDMG", "TOTKLD", "TOTINJ", "TRNSPD", "TONS" )])

# Cause
bwplot(Cause~ ACCDMG, main = "Box Plots of Accident Damage by Cause", xlab = "Damage ($)", ylab = "Accident Cause", data = xdmg)

# Type of accident
which(totacts$ACCDMG > dmgbox$stats[5])

#Use of jitter
bwplot(as.factor(YEAR)~jitter(ACCDMG, amount = 2.5e5), data = xdmg, main = "Box Plots of Extreme Accident Damage by Year (with jitter)", xlab = "Damage ($)", ylab = "Year")

par(mfrow = c(1,2))
boxplot(jitter(xdmg$ACCDMG, amount = 2.5e5), col = "steelblue", main = "Extreme Accident Damage with Jitter")
boxplot(xdmg$ACCDMG, col = "steelblue", main = "Extreme Accident Damage without Jitter")
par(mfrow = c(1,1))

# Conditioning on Cause
xyplot(ACCDMG~TRNSPD | Cause, main = "Extreme Damage vs. Train Speed Conditioned on Cause", xlab = "Train Speed", ylab = "Total Accident Damage", data = xdmg)


#****************************************
#			Heatmaps for categorical variables
#****************************************

table(xdmg$Cause, xdmg$TYPE)
heatmap(table(xdmg$Cause, xdmg$TYPE), Rowv = NA, Colv = NA)

source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xdmg$Cause, xdmg$TYPE), title = "No. of Accidents by Cause and Type of Accident")


#**********************
#			Data Cleaning
#**********************

# Look at the most costly accidents
which(xdmg$ACCDMG > 15e6)

# The max
which(xdmg$ACCDMG == max(xdmg$ACCDMG))

# Look at the narrative
as.matrix(names(xdmg))
xdmg[which(xdmg$ACCDMG == max(xdmg$ACCDMG)), 123:136]

# Are there other duplicates?
duplicated(xdmg[, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

# what about incident number?
which(xdmg$INCDTNO == "110058")
xdmg[which(xdmg$INCDTNO == "110058"),  c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]
duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

#remove the duplicates
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

dim(xdmgnd)
dim(xdmg)

# number of duplcates
nrow(xdmg) - nrow(xdmgnd)



#******************************************************
#					Session 5
#				
#				Principal Components
#******************************************************

# For Casualties (TOTINJ + TOTKLD)

xdmg$Casualties <- xdmg$TOTINJ + xdmg$TOTKLD

# Remove 9/11
which(xdmg$ACCDMG > 15e6)
xdmg <- xdmg[-191,]
xdmg$YEAR[which(xdmg$TOTKLD == max(xdmg$TOTKLD))]

#Reset rownames (observation #s) for sequential numbering- otherwise they will remain the #s from totacts
rownames(xdmgnd) <- NULL

#***********************************************************
#		Principal Components with the Correlation Matrix	
#***********************************************************

# Principal Components with the Correlation Matrix for extreme data 2 (metrics)
xdmgnd.pca <- princomp(xdmgnd[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)

# View data in the first 2 PC
biplot(xdmgnd.pca)

# Remove outliers in component 2
xdmgnd.pca <- princomp(xdmgnd[-c(5880,411),c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)

# View the first 2 PC without ouliers
biplot(xdmgnd.pca)

# Variance plot
screeplot(xdmgnd.pca, main = "Variance for PC of Metrics")

# Loadings
barplot(xdmgnd.pca$loadings[,1])
barplot(xdmgnd.pca$loadings[,2])

# Cumulative variance
setwd("/Users/sabreenaabedin/Desktop/class/SYS4021")
source("PCAplots.R")

cumplot(xdmgnd.pca, col = "blue")

#*********************************
#		Possible predictors of damage	
#*********************************

# SPM
source("SPM_Panel.R")
uva.pairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])

# PCA
pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )
biplot(pred.pca)


