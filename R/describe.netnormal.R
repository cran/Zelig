describe.netnormal<-function(){
category <- "continuous"
description  <- "Social Network Normal Regression for Continuous Dependent Variables"
package <-list(	name 	="stats",
		version	="0.1"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
