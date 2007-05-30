describe.nlme<-function(){
  category <- "continuous"
description  <- "Non linear regression model"

package <-list(	name 	="nlme",
		version	="3.1-75"
		)
parameters<-list(mu="mu",rho="rho")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=TRUE,
			depVar=TRUE,
			expVar=TRUE)
  
parameters$rho<-list(equations=c(1,1),
			tagsAllowed=TRUE,
			depVar=FALSE,
			expVar=TRUE)
  			
list(category=category,description=description,package=package,parameters=parameters)
}
