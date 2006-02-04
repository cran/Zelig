describe.poisson.bayes<-function(){
category <- "event.count"
description  <- "Ordinal Probit Regression for Ordered Categorical Dependent Variables"
package <-list(	name 	="MCMCpack",
		version	="0.6"
		)
parameters<-list(lambda="lambda")
parameters$lambda<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
