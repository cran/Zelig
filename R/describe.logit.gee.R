describe.logit.gee<-function(){
  category <- "dichotomous"
  description  <- "General Estimating Equation for Logistic Regression"
  package <- list(name ="gee",
		version	="4.13-12"
		)
  pi <- list(equations=c(1,1),
			tagsAllowed=FALSE,
			depVar=TRUE,
			expVar=TRUE
			)
			
  list(category=category,description=description,package=package,parameters=list(pi=pi))
}

