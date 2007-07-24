describe.ei.RxC <- function(){
category <- "ei"
description  <- "Hierarchical Multinomial-Dirichlet Ecological Inference Model for R x C Tables"
package <-list( name    ="stats",
                version ="0.1"
                )
parameters<-list(mu="mu")
parameters$mu<-list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE,
			specialFunction="cbind",
			varInSpecialFunction=c(2,Inf)
		)
			
list(category=category,description=description,package=package,parameters=parameters)
}
