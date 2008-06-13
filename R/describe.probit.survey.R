describe.probit.survey<-function(){
category <- "dichotomous"
description  <- "Probit Regression for Dichotomous Dependent Variables"
package <-list(	name 	="survey",
		version	="3.6-13"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
