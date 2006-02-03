describe.mlogit<-function(){
category <- "multinomial"
description  <- "Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values"
package <-list(	name 	="VGAM",
		version	="0.6",
		CRAN="http://www.stat.auckland.ac.nz/~yee"
		)
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,Inf),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="as.factor",
			varInSpecialFunction=c(1,1)
		)
			
list(category=category,description=description,package=package,parameters=parameters)
}
