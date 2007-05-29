describe.sur<-function(){
  category <- "continuous"
description  <- "Seemingly Unrelated Regression"

package <-list(	name 	="systemfit",
		version	="0.8"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(2,Inf),
			tagsAllowed=TRUE,
			depVar=TRUE,
			expVar=TRUE)
			
list(category=category,description=description,package=package,parameters=parameters)
}
