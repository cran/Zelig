describe.logit.bayes<-function(){
category <- "dichotomous"
description  <- "Bayesian Logistic Regression for Dichotomous Dependent Variables"
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(pi="pi")
parameters$pi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
