describe.ei.dynamic <- function(){
category <- "ei"
description  <- "Quinn's Dynamic Ecological Inference Model"
package <- list (
	name="MCMCpack",
	version="0.8-2"
	)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="cbind",
			varInSpecialFunction=c(2,Inf)
		)
			
list(category=category,description=description,package=package,parameters=parameters)
}
