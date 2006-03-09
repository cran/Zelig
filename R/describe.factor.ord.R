describe.factor.ord<-function(){
category <- "ordinal"
description  <- "Ordinal Data Factor Analysis"
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=FALSE,
			specialFunction="cbind",
			varInSpecialFunction=c(2,Inf)
		)
			
list(category=category,description=description,package=package,parameters=parameters)
}
