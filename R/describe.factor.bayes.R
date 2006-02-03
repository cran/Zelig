describe.factor.bayes<-function(){
category <- "censored"
description  <- "Bayesian Factor Analysis"
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
		)
			
list(category=category,description=description,package=package,parameters=parameters)
}
