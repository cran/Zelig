describe.tobit<-function(){
category <- "continuous"
description  <- "Linear regression for Left-Censored Dependet Variable"
package <-list(	name 	="survival",
		version	="2.2"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category=category,description=description,package=package,parameters=parameters)
}
