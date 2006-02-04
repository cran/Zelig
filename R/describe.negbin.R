describe.negbin<-function(){
category <- "event.count"
description  <- "Negative Binomial Regression for Event Count Dependent Variables"
package <-list(	name 	="MASS",
		version	="0.1"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
