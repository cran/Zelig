describe.exp<-function(){
category <- "bounded"
description  <- "Exponential Regression for Duration Dependent Variables"
package <-list(	name 	="survival",
		version	="2.0"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="Surv",
			varInSpecialFunction=c(2,2))
			
list(category=category,description=description,package=package,parameters=parameters)
}
