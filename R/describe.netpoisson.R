describe.poisson.net<-function(){
category <- "count"
description  <- "Social Network Poisson Regression for Event Count Dependent Variables"
package <-list(	name 	="sna",
		version	="1.4"
		)
parameters<-list(lambda="lambda")
parameters$lambda<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
