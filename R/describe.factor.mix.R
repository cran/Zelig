describe.factor.mix<-function(){
category <- "censored"
description  <- "Mixed Data Factor Analysis"
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=FALSE,
			specialFunction="cbind",
			varInSpecialFunction=c(1,Inf)
		)
			
list(category=category,description=description,package=package,parameters=parameters)
}
