describe.default<-function(){
category <- "Dichotomous"
description  <- "A statistical model"

parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE)
			
list(category=category,description=description,parameters=parameters)
}
