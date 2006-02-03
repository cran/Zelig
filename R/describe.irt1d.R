describe.factor.irt1d<-function(){
category <- "censored"
description  <- "One Dimensional Item Response Model"
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(pi="pi")
parameters$pi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=FALSE,
			specialFunction="cbind",
			varInSpecialFunction=c(1,Inf)
		)
			
list(category=category,description=description,package=package,parameters=parameters)
}
