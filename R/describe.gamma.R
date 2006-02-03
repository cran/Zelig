describe.gamma<-function(){
category <- "continuous"
description  <- "Gamma Regression for Continuous, Positive Dependent Variables"
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
