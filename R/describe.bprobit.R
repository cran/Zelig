describe.bprobit<-function(){
category <- "dichotomous"
description  <- "Bivariate Probit Regression for Dichotomous Dependent Variables"
package <-list(	name 	="VGAM",
		version	="0.6",
		CRAN    ="http://www.stat.auckland.ac.nz/~yee"
		)
parameters<-list(mu="mu", rho="rho")
parameters$mu<-list(equations=c(2,2),
			tagsAllowed=TRUE,
			depVar=TRUE,
			expVar=TRUE)
			
parameters$rho<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=FALSE,
			expVar=TRUE)
list(category=category,description=description,package=package,parameters=parameters)
}
