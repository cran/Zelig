describe.oprobit.bayes<-function(){
category <- "ordinal"
description  <- "Bayesian Ordered Probit Regression"
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="as.factor",
			varInSpecialFunction=c(1,1)
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
