library(boot)

#****************************************************
# 		
#			Coefficients in a regression with 
#				residuals modeled as an AR
#				process
#					 coef.fun
#
#****************************************************

coef.fun <- function(data, indices, fit, resid, X2)
{
	est <- fit + resid[indices]
	model <- lm(est~X2-1)
	coefficients(model)
	
	}


#****************************************************
# 		
#			Bootstrapping the  regression
#					RTSB
#				(bootstrap residuals)
#****************************************************

#  	resp: the response
#	pred: the predictor(s)
#	fit: the fit of the linear model on the data
#	resid: the residual from the fit without the time series
#	X: the model with the predictors and time series.
# 	num: number of bootstrap replicates


RTSB <- function(resp, pred, fit, resid, X, num)
{
		boot(cbind(resp, pred), coef.fun, num, fit = fit, resid = resid, X2 = X)

	}



#****************************************************
# 		
#			Predicing the next event in an AR Process
#					tspred
#
#****************************************************


# This function is the statistic for bootstrapping
# In this case it is the one step prediction for the AR process

tspred.fun <- function(tsb, tsnew) 
{    
	ar.fit <- ar(tsb, order.max=2)
    ar.pred <- predict(ar.fit, newdata = tsnew)
    ar.pred$pred[[1]]
    #ar.fit$ar
     
}





#****************************************************
# 		
#			Simulating the AR Process
#					ts.sim
#
#****************************************************


# This function outputs a simulation for the AR process
#  res:  residuals
#  n.sim: number of simulations (normally the same as 
#	the original process)
#	ran.args: additional arguments to the simulation



ts.sim <- function(res,n.sim, ran.args) {
     rg1 <- function(n, res)
          sample(res, n, replace=TRUE)
     ts.orig <- ran.args$ts
     ts.mod <- ran.args$model
     mean(ts.orig)+ts(arima.sim(model=ts.mod, n=n.sim,
                      rand.gen=rg1, res=as.vector(res)))
}





#****************************************************
# 		
#			Bootstrapping the  AR Process
#					TSB
#
#****************************************************


# 	This function runs the boot strap simulation
# 	and outputs the bootstrap statistics
#  	tsint:  the time series to be bootstrapped
#  	oth.arg: the new data to use for the forecast 
#	for an AR(1) process it is a scalar, but for 
#	higher order AR processes it is a vector with 
#	the number of elements to match the order of the 
#	process.



TSB <- function(tsint, oth.arg,boot.number=1000)
{
	
ts.ar <- ar(tsint)
ts.model <- list(order=c(ts.ar$order,0,0),ar=ts.ar$ar)
ts.res <- ts.ar$resid[!is.na(ts.ar$resid)]
ts.res <- ts.res - mean(ts.res)
tsboot(ts.res, tspred.fun, R= boot.number, sim="model", n.sim=3*length(tsint),
                 orig.t=FALSE, ran.gen=ts.sim, 
                 ran.args=list(ts=tsint, model=ts.model), tsnew = oth.arg)
	}


#****************************************************
# 		
#			Confidence Intervals for Output from
#					TSB
#
#****************************************************

# This function produces confidence intervals for TSB output

tsboot.ci <- function(b.obj, inter = c(.05, .95))
{
	c.05 <- quantile(b.obj$t, inter[1])
	c.95 <- quantile(b.obj$t, inter[2])
	c(c.05, c.95)
}


#****************************************************
# 		
#			Bootstrapping the Forecast from a Regression
#					RFB
#				(bootstrap residuals)
#****************************************************

#  	resp: the response
#	pred: the predictor(s)
#	fit: the fit of the linear model on the data
#	resid: the residual from the fit without the time series
#	X: the model with the predictors and time series.


RFB <- function(DM, model, ndata, num = 2000)
{
		boot(DM, pred.func, R= num, model = model, ndata = ndata)

	}
	
	
pred.func <- function(data, indices, model, ndata)
{
	if(!(is.data.frame(data))){data <- as.data.frame(data)}
	est <- fitted(model) + residuals(model)[indices]
	my.form <-  grep("~" , formula(model), value = T, invert = T)
	nmod <- lm(formula(paste( "est ~ ", my.form[2])), data)
	predict(nmod, ndata)
	
}

# This code is old use RFBSIM2

RFBSIM <- function(DM, model, ndata, num = 2000)
{
		boot(DM, pred.func.sim, R= num, model = model, ndata = ndata)

	}


pred.func.sim <- function(data, indices, model, ndata)
{
	if(!(is.data.frame(data))){data <- as.data.frame(data)}
	est <- fitted(model) + residuals(model)[indices]
	my.form <-  grep("~" , formula(model), value = T, invert = T)
	nmod <- lm(formula(paste( "est ~ ", my.form[2])), data)
	Nc <- ncol(ndata)
	ND <- data.frame(matrix(nrow =1, ncol = Nc))	
	colnames(ND) <- colnames(ndata)
	for(i in 1:Nc)
	{
		ND[1,i] <- rnorm(1, mean = ndata[1,i], sd =ndata[2,i])
	}
	predict(nmod, ND)
	
}

# RFBSIM2 assumes the variables to be simulated are ordered first the data frames, DM and ndata.

RFBSIM2 <- function(DM, model, ndata, num = 2000, predse)
{
		boot(DM, pred.func.sim2, R= num, model = model, ndata = ndata, predse = predse)

	}


#pred.func.sim2 <- function(data, indices, model, ndata, predse)
#{
#	if(!(is.data.frame(data))){data <- as.data.frame(data)}
#	est <- fitted(model) + residuals(model)[indices]
#	my.form <-  grep("~" , formula(model), value = T, invert = T)
#	nmod <- lm(formula(paste( "est ~ ", my.form[2])), data)
#	Nc <- ncol(predse)
#	ND <- ndata
#	predict(nmod, ND)
	
#}



pred.func.sim2 <- function(data, indices, model, ndata, predse)
{
	if(!(is.data.frame(data))){data <- as.data.frame(data)}
	est <- fitted(model) + residuals(model)[indices]
	my.form <-  grep("~" , formula(model), value = T, invert = T)
	nmod <- lm(formula(paste( "est ~ ", my.form[2])), data)
	Nc <- ncol(predse)
	ND <- ndata	
	for(i in 1:Nc)
	{
		ND[1,i] <- rnorm(1, mean = ndata[1,i], sd =predse[2,i])
	}
	predict(nmod, ND)
	
}

#************
# This function is not tested. Needs work


RFBSIM3 <- function(DM, model, ndata, num = 2000, predse, SimI )
{
		boot(DM, pred.func.sim3, R= num, model = model, ndata = ndata, predse = predse)

	}



pred.func.sim3 <- function(data, indices, model, ndata, predse, SimI = SimI)
{
	if(!(is.data.frame(data))){data <- as.data.frame(data)}
	est <- fitted(model) + residuals(model)[indices]
	my.form <-  grep("~" , formula(model), value = T, invert = T)
	nmod <- lm(formula(paste( "est ~ ", my.form[2])), data)
	Nc <- ncol(predse)
	ND <- ndata	
	for(i in 1:Nc)
	{
		ND[1,i] <- rnorm(1, mean = ndata[SimI,i], sd =predse[2,i])
	}
	predict(nmod, ND)
	
}









#****************************************************
# 		
#			Bootstrapping the  AR Forecast
#					TSF
#
#****************************************************


# 	This function runs the boot strap simulation
# 	and outputs the bootstrap statistics
#  	tsint:  the time series to be bootstrapped
#  	oth.arg: the new data to use for the forecast 
#	for an AR(1) process it is a scalar, but for 
#	higher order AR processes it is a vector with 
#	the number of elements to match the order of the 
#	process.



TSF <- function(tsint, oth.arg,boot.number=1000, n.ahead = 1, func = "M")
{
	
ts.ar <- ar(tsint)
ts.model <- list(order=c(ts.ar$order,0,0),ar=ts.ar$ar)
ts.res <- ts.ar$resid[!is.na(ts.ar$resid)]
ts.res <- ts.res - mean(ts.res)
if(func == "M" ){tsboot(ts.res, tspred.mean, R= boot.number, sim="model", n.sim=3*length(tsint),
                 orig.t=FALSE, ran.gen=ts.sim, 
                 ran.args=list(ts=tsint, model=ts.model), tsnew = oth.arg, n.ahead = n.ahead)}
else{tsboot(ts.res, tspred.se, R= boot.number, sim="model", n.sim=3*length(tsint),
                 orig.t=FALSE, ran.gen=ts.sim, 
                 ran.args=list(ts=tsint, model=ts.model), tsnew = oth.arg, n.ahead = n.ahead)}

	}




# This function is the statistic for bootstrapping
# In this case it is the one step prediction for the AR process

tspred.se <- function(tsb, tsnew, n.ahead = 1) 
{    
	ar.fit <- ar(tsb, order.max=2)
    ar.pred <- predict(ar.fit, newdata = tsnew, n.ahead = n.ahead)
    ar.pred$se
    #ar.fit$ar
     
}

tspred.mean <- function(tsb, tsnew, n.ahead = 1) 
{    
	ar.fit <- ar(tsb, order.max=2)
    ar.pred <- predict(ar.fit, newdata = tsnew, n.ahead = n.ahead)
    ar.pred$pred
    #ar.fit$ar
     
}

