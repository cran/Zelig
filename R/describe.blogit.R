describe.blogit<-function(){
category <- "dichotomous"
description  <- "Bivariate Logistic Regression for Dichotomous Dependent Variables"
package <-list(	name 	="VGAM",
		version	="0.6",
		CRAN    ="http://www.stat.auckland.ac.nz/~yee"
		)
parameters<-list(mu="mu",phi="phi")
parameters$mu<-list(equations=c(2,2),
			tagsAllowed=TRUE,
			depVar=TRUE,
			expVar=TRUE)
			
parameters$phi<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=FALSE,
			expVar=TRUE)
list(category=category,description=description,package=package,parameters=parameters)
}
