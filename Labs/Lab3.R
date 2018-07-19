######### LAB 3 ###########
totacts$Casualty = totacts$TOTKLD + totacts$TOTINJ

#with at least one casualty
casbox <- boxplot(totacts$Casualty > 0)

#extreme causalty
xcas <- totacts[totacts$Casualty > casbox$stats[5],]

#proportion of casualties
length(casbox$out)/nrow(totacts)

#extreme casualty no duplicates
xcasnd <- xcas[!(duplicated(xcas[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

#check the difference
dim(xcas)
dim(xcasnd)
nrow(xcas)-nrow(xcasnd)

#boxplot by year
bwplot(as.factor(YEAR)~Casualty, data = xcasnd, ylab = "Year")

#sum of casualties per year
plot(1:16, tapply(xcasnd$Casualty, xcasnd$YEAR, sum), type = "l", xlab = "Year", ylab = "Casualty", main = "Total Casualties per Year")

#which variable had the highest number of accidents
colSums(table(xcasnd$Casualty, xcasnd$Cause))
colSums(table(xcasnd$Casualty, xcasnd$TYPE))
colSums(table(xcasnd$Casualty, xcasnd$YEAR))
colSums(table(xcasnd$Casualty, xcasnd$TYPEQ))

#which cause has the accident with the greatest number
bwplot(as.factor(Cause)~Casualty, data = xcasnd)

# total number of casualties
sum(xcasnd$Casualty)

which(xcasnd$Casualty == max(xcasnd$Casualty))
xcasnd[205,c("TOTKLD", "TIMEHR", "AMPM", "TYPEQ", "CARSHZD")]
xcasnd <- xcasnd[-205,]

#finding highest correlation
cor(xcasnd$Casualty, xcasnd$TRNSPD)
cor(xcasnd$Casualty, xcasnd$CARS)

#principal component
xcasnd.pca <- princomp(xcasnd[,c("TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T)
biplot(xcasnd.pca)
summary(xcasnd.pca)
xcasnd.pca$loadings



