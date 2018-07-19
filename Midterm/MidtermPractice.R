### Midterm Practice Work

##Load pima-inidans-diabetes.csv and housing.csv



source("SPM_Panel.r")
source("PCAplots.R")
source("FactorPlots.r")
source("ROC.R")


#% ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#%              Q1:  Linear Regression Using Housing Data
#% ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
##You have been hired by a real estate company to build models for pricing homes. 
##The company has data for 83 homes. For the analysis use the data in housing.csv.

##These Data have the following variables- 
##sqft, price, City, bedrooms, baths
#% ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

##Q1a Use a box plot indicate whether the price variable has any outliers. If, so how many does it have?

##Q1b Find the values for the lower and upper whisker of Price. What are these values?

##Q1c Use a scatter plot matrix or individual scatter plots and list the variable in the data set 
##    that has the strongest linear relationship to price.

##Q1d Build a main effects model using sqft, bedrooms, and baths. Write this model out mathematically 
##    as well. The response variable is price.

##Q1e Build a model that includes the main effect and all interaction terms using sqft, bedrooms, and baths. 
##      Write this model out mathematically as well.  The response variable is price.

##Q1f Write the hypothesis for the partial F test to compare the models you found for questions
##    1d and 1e. Conduct this test and report your results and recommendation.  What is the 
##    p-value (to 2 significant digits)?  Do we accept or reject the null hypothesis. Which model 
##    do we choose?

##Q1g-- Use both AIC and adjusted $R^2$ to compare the interaction model obtained in question 1e with 
##      the main effects model using from 1d. List the values you find for this comparison and your 
##      conclusion regarding your choice of model. (Hint: use summary() to find adjusted $R^2$ and 
##      AIC() to find AIC.)

##Q1h--  Do you observe any problems in a residuals vs. fitted plot for the main effects model? 
##        If so, what are they?

##Q1i-- Do you observe any problems in the Residuals vs Leverage plot?  If so, what are they, and 
##      what are the observation numbers affected?

##Q1j-- (2) Using Box-Cox plot would you recommend a transformation of the response variable? 
##          If so, what transformation?

##Q1k-- Find the principal components for the variables price, sqft, bedrooms, and baths. Why do we leave out 
##      the other variables? Plot the biplot (biplot()) or the loadings plot (loadingplot() with PCAplots.R 
##      sourced) and state which 2 variables have the largest absolute loadings in the first principal component. 

##Q1l-- How many dummy variables do you need to encode City?

#% ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#%            Q2:  GLM using Pima Indians Data
#% ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
## You have been hired by a company to design a system to predict 
## whether a sample of female patients have diabetes or not.

##You have data from a retrospective sample of data from female Pima Indians on diabetes.
##The data set has 9 variables shown below and the last variable, class, is the response. 
##The data are posted on the Collab site and are in the attachment pima-indians-diabetes.csv.  
##The attributes are:

## pregnancies - Number of times pregnant
## plasmaglucose - Plasma glucose concentration a 2 hours in an oral glucose tolerance test
## dbp - Diastolic blood pressure (mm Hg)
## tricep - Triceps skin fold thickness (mm)
## insulin - 2-Hour serum insulin (mu U/ml)
## bmi - Body mass index (weight in kg/(height in m)^2)
## diabetesped - Diabetes pedigree function
## age - Age (years)
## class - Diabetes Class variable (0 or 1)
#% ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

##Q2a Assume the probability of the class (diabetes) can be modeled with a logistic function. 
##    Write a main effects logistic regression or generalized linear model for the log odds of diabetes
##    using bmi, age, and insulin.


##Q2b Write the null model we use for the model utility test. 


##Q2c Perform the model utility test for the model. Do you reject the null hypothesis at the 0.05 level?
##    Give the significance  level you obtained for this test.


##Q2d In your model from question 2a interpret the coefficient you found for bmi in terms of 
##    the percentage change in the odds of diabetes for each additional unit increase in BMI.

##Q2f     Add the rest of the predictor variables to your model from question 2a. Test the significance of 
##        the resulting model using a likelihood test with a $\chi^2$ statistic. What significance 
##        level do you obtain for your test and what does this value imply with regard to which 
##        model you should use?


##Q2g Suppose that our clients only want to know if age and bmi alone predicts diabetes.
##    We can use either the main effects model we found in question 2f or a model that 
##    only has age and bmi as a predictor. 
##    Which model should we use to answer their question and why should we chose it?


##Q2h Now build a full main effects + interaction model with all predictors.  
##    Apply step() to your model.  How many main effects terms are in your 
##    resultant stepwise regression model?  Interaction Terms?


##Q2i Use the stepwise selected model you found in question 2h to 
##    obtain predicted values for the entire data set you used to 
##    estimate the model. (Hint: the use of this data set is the default in R with the 
##    predict() function.) 
##    Now find the confusion matrix or score table with a threshold of 0.5. 
##    List the total errors, the false positive errors, and the false negative 
##    errors for this model.





