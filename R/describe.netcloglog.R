describe.cloglog.net<-function(){
category <- "dichotomous"
description  <- "Social Network Complementary Log Log Regression for Dichotomous Dependent Variables"
package <-list(	name 	="sna",
		version	="1.4"
		)
parameters<-list(pi="pi")
parameters$pi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
