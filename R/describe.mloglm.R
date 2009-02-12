describe.mloglm<-function(){
category <- "multinomial"
description  <- "Multinomial Log-Linear Regression for Contingency Table Models"
authors <- c()
year <- 2007
package <-list(	name 	="nnet",
		version	="0.6"
		)

parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
list(category = category, authors = authors, year = year,description=description,package=package,parameters=parameters)
}
