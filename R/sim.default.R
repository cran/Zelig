sim.default <- function(object, x, x1=NULL, num=c(1000, 100),
                        prev = NULL, bootstrap = FALSE, bootfn=NULL,
                        cond.data = NULL, ...) {
  if (any(class(object) == "MCMCZelig"))
    num <- nrow(object$coefficients)
  else if (length(num) == 2) {
    if (!bootstrap)
      num <- num[1]
    else
      num <- num[2]
  }
  if (is.null(prev)) {
    if (!bootstrap & any(class(object) != "relogit"))
      simpar <- param(object, num=num, bootstrap=bootstrap)
    else if (any(class(object) == "relogit")) 
      simpar <- param.relogit(object, num=num, x=x, bootstrap=bootstrap, bootfn=bootfn, ...) 
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
  simqi <- qi(object, simpar = simpar, x = x, x1 = x1, y = NULL)
  c <- match.call()
  c$num <- num
  res <- list(x=x, x1=x1, call = c, zelig.call = object$call,
              par = simpar, qi=simqi$qi, qi.name=simqi$qi.name)
  class(res) <- "zelig"
  res
}
















