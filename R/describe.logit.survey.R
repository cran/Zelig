describe.logit.survey<-function(){
category <- "dichotomous"
description  <- "Survey-Weighted Logistic Regression for Dichotomous Dependent Variables"
package <-list(	name 	="survey",
		version	="3.6-13"
		)
parameters<-list(pi="pi")
parameters$pi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
