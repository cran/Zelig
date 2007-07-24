zelig2netpoisson <- function(formula, model, data, M, ...) {
  check <- library()
  if(any(check$results[,"Package"] == "sna")) 
    require(sna)
  else
        stop("Please install sna using \n	install.packages(\"sna\")")
	mf <- match.call(expand.dots = TRUE)
	mf[[1]] <- as.name("netpoisson")	
	mf$M <- mf$model  <- NULL
	mf$formula <- formula
	as.call(mf)	}