describe.gamma.gee<-function(){
  category <- "bounded"
  description  <- "General Estimating Equation for Gamma Regression"
  package <- list(name ="gee",
		version	="4.13-12"
		)
  lambda <- list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
  list(category=category,description=description,package=package,parameters=list(lambda=lambda))
}

