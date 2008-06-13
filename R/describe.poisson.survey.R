describe.poisson.survey<-function(){
category <- "count"
description  <- "Survey-Weighted Poisson Regression for Event Count Dependent Variables"
package <-list(	name 	="survey",
		version	="3.6-13"
		)
parameters<-list(lambda="lambda")
parameters$lambda<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
