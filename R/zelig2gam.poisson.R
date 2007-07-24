zelig2gam.poisson <- function(formula, model, data, M, ...) {
  check <- library()
  if(any(check$results[,"Package"] == "mgcv")) 
    require(mgcv)
  else
        stop("Please install mgcv using \n	install.packages(\"mgcv\")")
	mf <- match.call(expand.dots = TRUE)
	mf[[1]] <- as.name("gam")	
	mf$M <- mf$model  <- NULL
	class(formula) <- "gamF"
	mf$formula <- formula
	mf$family <- poisson()
	as.call(mf)	}