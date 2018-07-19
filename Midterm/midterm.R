# QUESTION 1
app <- app

appbox <-boxplot(app$Price)
appbox
app$Platform <- factor(app$Platform)
contrasts(app$Platform)

uva.pairs(app[,c("Advert", "Platform","Develop","X5Star", "CompSites", "Date",  "Users", "Price")])

devplot <- plot(app$Price, app$Develop)
summary(devplot)

mainmodel<-lm(Price~Develop + Advert + Platform + X5Star + CompSites + Date + Users ,data=app)
interactionmodel<-lm(Price~(Develop + Platform)^2,data=app)

AIC(mainmodel)
AIC(interactionmodel)
summary(mainmodel)
summary(interactionmodel)

par(mfrow=c(2,2))
plot(mainmodel, labels.id = NULL)
par(mfrow=c(1,1)) 

library(MASS)
boxcox(mainmodel) 
boxcox(mainmodel, plotit = F)$x[which.max(boxcox(mainmodel, plotit = F)$y)] 


main.pca <- princomp(app[,c("Develop","X5Star", "CompSites", "Date",  "Users", "Price")], cor = T)
biplot(main.pca)
loadings(main.pca)

# QUESTION 2

prognostic$ca <- as.factor(prognostic$ca)
contrasts(prognostic$ca)

contrasts(prognostic$ca)<-matrix(c( 0,1,1,0),nrow=2)
as.factor(prognostic.ca)

main <- glm(death~log(ca+age), data = prognostic, family = binomial )  
main.null <- glm(death~1, data = prognostic, family = binomial)
anova(all, main, test = "Chi")
main

all <- glm(death~. , data = prognostic, family = binomial)
