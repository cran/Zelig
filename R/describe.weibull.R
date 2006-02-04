describe.weibull<-function(){
category <- "censored"
description  <- "Weibull Regression for Duration Dependent Variables"
package <-list(	name 	="survival",
		version	="2.2"
		)
parameters<-list(lambda="lambda")
parameters$lambda<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="Surv",
			varInSpecialFunction=c(2,2)
		)
			
list(category=category,description=description,package=package,parameters=parameters)
}
