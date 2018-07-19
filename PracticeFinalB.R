# QUESTION 1

# part a
head(airquality)
boxplot(airquality$Ozone)

# part b
df.glm <- glm(Ozone~., data = airquality)
plot(df.glm, which=2) # q-q plot

# part c
uva.pairs(airquality[,c("Ozone", "Solar.R", "Wind", "Temp", "Month", "Day")])

# part d
pred.pca <- princomp(na.omit(airquality[,c("Ozone","Solar.R", "Wind", "Temp", "Month", "Day")]), cor = T ) # calculate principal components
biplot(pred.pca) # graph biplot

# part e
loadings(pred.pca) # Ozone and temp have high loadings in component 1 and low (or no) loadings in component 2

# part f - about 3.5

# part g
aq.lm <- lm(Ozone~., data=airquality)

# part h
summary(aq.lm) # suggests that model is significant - all predictors can be used to predict ozone

# part i - does this mean make conclusions based on the parts visible?


# part j - non-constant variance, not normal, no outliers which is good
  # for non-constant variance ?
  # for non-normality, transform based on box-cox plot

# part k: Ozone = b0 + b1*S + b2*T + b3+M + b4*D + b5*S*W + b6*S*T ...

# part l
aq.interactions <- lm(Ozone~ Solar.R + Wind + Temp + Month + Day + (Solar.R + Wind + Temp + Month + Day)^2, data = airquality)
summary(aq.interactions)

# part m: hypothesis = (null) the smaller model is better (alt) the larger model is significantly better

# part n:
anova(aq.lm,aq.interactions) 

# part o: significant at 0.05 level, use the larger model (interactions plot)


# QUESTION 2

# a: prestige = -6.0647 +  0.5987 * income + 0.5458 * education
# b: n - k - 1 = 42, n = 45 total observations
# c: ?
# d: 13.37 - residual standard error ?
# e: job.lm <- lm(prestige ~ education + income + type.bc + type.wc, data = USCensus)
# f: run a partial f-test with the model and without it - anova(original.lm, job.lm)
# g: it's a lm so could bootstrap parameters, residuals, or observations - could be used to get CI since we don't know distributions


# QUESTION 3

# a: 
df.glm.main <- glm(log(Kyphosis)~., data = df, family = binomial)

# b: use model utility test
df.null <- glm(log(Kyphosis)~1, data = df, family = binomial)
anova(df.null, df.glm.main, test = "Chi") # using Chi2 statistic

# c: The larger model is significantly better than the null model (proportional model)
# d: create interaction model and run partial F test??


