describe.ls.net<-function(){
category <- "continuous"
description  <- "Social Network Least Squares Regression for Continuous Dependent Variables"
package <-list(	name 	="sna",
		version	="1.4"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
