describe.arima<- function(){
category<- "continuous"
description <- "Arima models for Time Series Data"
package <-list( name    ="stats",
                version ="0.1"
                )
mu<- list(equations=c(1,1),
	tagsAllowed=FALSE,
	depVar=TRUE,
	expVar=TRUE,
	specialFunction="Diff",
	varInSpecialFunction= 4)
sigma2<- list(equations=c(1,1),
	tagsAllowed=FALSE,
	depVar=FALSE,
	expVar=FALSE)
pars<- list(mu=mu, sigma2=sigma2)
model<- list(category=category,description=description,package=package, parameters=pars)
}
