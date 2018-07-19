# QUESTION 3

# PART A
df <-read.table('trainkld.txt',header=T,sep=',')

df.ts<-ts(df$x) # create a time series
summary(df.ts)
plot(df.ts, type = "l") # plot general trend over time
abline(df.trend, col = "red") # add trendline

pg.df<-spec.pgram(df.ts,spans=15,demean=T,log='no')
max.omega.ham<-pg.df$freq[which(pg.df$spec==max(pg.df$spec))] # Find the peak
1/max.omega.ham # Period
df.ts2 = ts.intersect(df.ts, newdata=lag(df.ts,-7), dframe=TRUE) # can build new model based on lag (period)

time.df<-c(1:(length(df.ts))) # create a time variable

df.trend<-lm(df.ts[time.df]~time.df) # TREND: lm of response over time
summary(df.trend)

# PART B
df.auto <- auto.arima(df.ts, trace=TRUE)


# PART C
e.ts.df<-ts(df.auto$residuals) # get residuals
plot(e.ts.df, ylab = "Residuals")

# PLOT ACF AND PACF
par(mfrow=c(1,2))
acf(e.ts.df, main="ACF of Residuals from spam.trend")
pacf(e.ts.df,main="PACF of Residuals from spam.trend")
par(mfrow=c(1,1))





# QUESTION 1

# part a

df <- read.table("housing-prices.csv", sep = ",", header = T)
box <-boxplot(df$Price)
df.outliers <- df[df$Price > box$stats[5],] # get outliers

#part b
uva.pairs(df[])

#part d
df.lm<-lm(Price~ Size, data=df) # can use . to use all variables
summary(df.lm) # summary and p-value
names(df.lm)
coef(df.lm)

#part e
lm.main<-lm(Price~ ., data=df) # can use . to use all variables
summary(lm.main) # summary and p-value

# part f
lm.interactions <- lm(Price ~ Age + Size + (Age + Size)^2, data = df)

# part h
summary(lm.interactions)
anova(df.lm, lm.interactions)

# part i
lm.complete.first <- lm(Price ~ . + (.)^2, data = df)
summary(lm.complete.first)
df.lm.step<-step(lm.complete.first) # start with a lm
summary(df.lm.step)
AIC(df.lm.step) # AIC

# part j & k
par(mfrow=c(2,2))
plot(df.lm.step, labels.id = NULL)
par(mfrow=c(1,1)) 

# part l
# BOX-COX PLOT - transformation
boxcox(df.lm.step, data = df) # box-cox plot
boxcox(df.lm.step,plotit=F, data = df) # just values

max(boxcox(df.lm.step, plotit = F, data = df)$y) # find y with max labmda
boxcox(df.lm, plotit = F, data = df)$x[which.max(boxcox(df.lm, plotit = F, data = df)$y)] # best labmda
L<-boxcox(df.lm, plotit = F, data = df)$x[which.max(boxcox(df.lm, plotit = F, data = df)$y)] # store as L

df.lm.boxcox<-lm(RESPONSE^L ~COL1+COL2,data=df) # use L in model
summary(df.lm.boxcox)
plot(density(df$RESPONSE^L))

# part m
df.selected<-df[,c('Price', 'Rooms', 'Baths', 'Size')] #no response variable here.
df.pc<-princomp(df.selected,cor=T) # get principal components
biplot(df.pc)
loadings(df.pc)
loadingsplot(df.pc)

# part n
var.comp(df.pc,98)

# part o
df.kfit <- fitted(df.lm)
df.ke <- residuals(df.lm)
df.mod <- model.matrix(df.lm)
df.boot <- RTSB(df$Price, df$Size, df.kfit, df.ke, df.mod,5000)
# outcome variable, input variable, fitted, residuals, parameters, # replications
uva.kidney.boot$t
sqrt(abs(var(uva.kidney.boot$t)))
df.boot

# part q
#    95% CI of r11donor
boot.ci(df.boot, .95, index=2)






# QUESTION 2

# part a
df<-read.table('heart.csv',header=T,sep=',')
df$cp <- factor(df$cp)
df$fbs <- factor(df$fbs)

df.glm.main <- glm(diag~ cp + age, data = df, family = binomial)
summary(df.glm.main)

# part b
df.null <- glm(diag~1, data = df, family = binomial)
anova(df.null, df.glm.main, test = "Chi")

# part e
df.glm.e <- glm(diag~ cp + age + restbps + chol + fbs, data = df, family = binomial)

anova(df.glm.e, df.glm.main, test = "Chi")

# part f
df.chol <- glm(diag~ cp, data = df, family = binomial)
anova(df.glm.efake, df.chol, test = "Chi")
anova(df.chol, df.null, test = "Chi")

# part g
df$restecg <- factor(df$restecg)
df$sex <- factor(df$sex)

df.glm.all <- glm(diag~. ,data = df, family = binomial)
df.step <- step(df.glm.all, trace = 0) # main effects stepwise
df.step

# part h
df.pred <- predict(df.step, newdata = df) # get predictions - glm was made with training data
score.table(df.pred, df$diag, .5) 

# part i
plot.roc(df.pred, df$diag, main = "ROC Curve", col = "blue")
legend(.6, .57, legend = c("Model1", "Model2"), col = c("blue", "orange", "green", "red", "cyan"), lwd = 1)

# part j
lm1 <- glmer(diag~ age + (1|sex), data = df, family = binomial)
lm2 <- glmer(diag~ (age|sex) + (age|sex), data = df, family = binomial)
anova(lm1, lm2, test= "Chi")
