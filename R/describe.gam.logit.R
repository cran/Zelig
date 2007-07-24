describe.gam.logit<-function(){
category <- "dichotomous"
description  <- "Generalized Additive Model for Dichotomous Dependent Variables"
package <-list(	name 	="mgcv",
		version	="0.1"
		)
parameters<-list(pi="pi")
parameters$pi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
