describe.ei.hier <- function(){
category <- "ei"
description  <- "Hierarchical Ecological Inference Model for  2 x 2 Tables"
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
