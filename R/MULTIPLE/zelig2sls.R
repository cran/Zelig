zelig2sls <- function(formula, model, data, M, constrain = NULL,
                          omit = NULL, constant = 3, ...) {
  check <- library()
  if(any(check$results[,"Package"] == "systemfit")) 
    require(systemfit)
  else
    stop("Please install systemfit using ....")
  mf <- match.call(expand.dots = TRUE)
  mf[[1]] <- as.name("callsystemfit")
  #mf$family <- as.name("bprobit")
  tmp <- cmsystemfit(formula, omit)
  mf$eqns <- tmp
  mf$method<-"WLS"
  mf$model <- mf$constant <- mf$M<-NULL
  as.call(mf)
}
