describe.poisson<-function(){
category <- "count"
description  <- "Poisson Regression for Event Count Dependent Variables"
package <-list(	name 	="stats",
		version	="0.1"
		)
parameters<-list(lambda="lambda")
parameters$lambda<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
