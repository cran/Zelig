sim.cond <- function(object, x, x1=NULL, num=c(1000, 100),
                     qoi = c("ev", "pr"), prev = NULL,
                     bootstrap = FALSE, bootfn=NULL,
                     cond.data = NULL, ...) {
  if (!is.null(x1)) {
    warning("First Differences are not calculated in conditional prediction models.")
    x1 <- NULL
  }  
  if (object$call$model %in% c("bprobit", "blogit")) {
    yvar <- x[, 1:2]
    x <- x[, 3:ncol(x)]
  }
  else if (any(class(object) == "EI")) {
    yvar <- x[, 1:object$dims[2]]
    x <- x[, (object$dims[2]+1):ncol(x)]
  }
  else if (any(class(object) == "latent")) 
    yvar <- NULL
  else {
    yvar <- x[,1]
    x <- x[,2:ncol(x)]
  }
  class(x) <- c("cond")
  if (any(class(object) == "MCMCZelig"))
    num <- nrow(object$coefficients)
  if (length(num) == 2) {
    if (!bootstrap)
      num <- num[1]
    else
      num <- num[2]
  }
  if (is.null(prev)) {
    if (!bootstrap & any(class(object) != "relogit")) 
      simpar <- param(object, num=num, bootstrap=bootstrap)
    else if (any(class(object) == "relogit")) 
      simpar <- param.relogit(object, num=num, x=x,
                              bootstrap=bootstrap, bootfn=bootfn, ...) 
    else {
      tt <- terms(object)
      dta <- eval(object$data, sys.parent())
      dta <- dta[complete.cases(model.frame(tt, dta)),]
      if (is.null(bootfn)) 
        bootfn <- bootfn.default
      res <- boot(dta, bootfn, R = num, object = object, ...)
      colnames(res$t) <- names(res$t0)
      simpar <- res$t
    }
  }
  else {
    if (bootstrap)
      stop("Error: Choosing 'bootstrap = TRUE' generates new parameters.  \nIf you wish to use previously generated parameters, \nplease specify only 'prev'.")
    else
      simpar <- prev
  }
  if (class(object)[1] == "survreg") 
    simqi <- qi(object, simpar = simpar, x = x, x1 = x1, y = yvar,
                cond.data = cond.data)
  else
    simqi <- qi(object, simpar = simpar, x = x, x1 = x1, y = yvar)
  class(x) <- "cond"
  c <- match.call()
  c$num <- num
  res <- list(x=x, x1=x1, call = c, zelig.call = object$call,
              par = simpar, qi=simqi$qi, qi.name=simqi$qi.name)
  class(res) <- "zelig"
  res
}







