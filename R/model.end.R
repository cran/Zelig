model.end <- function(Terms, res) {

  variance <- -solve(res$hessian)

  lev <- eval(as.name(attr(Terms, "items")$lev), envir = parent.frame())
  kall <- eval(as.name("kall"), envir = parent.frame())   
  xlev <- eval(as.name(attr(Terms, "items")$xlev), envir = parent.frame())
  mf <- eval(as.name(attr(Terms, "items")$mf), envir = parent.frame())
  x <- eval(as.name(attr(Terms, "items")$x), envir = parent.frame())
  
  names(res$par) <- c(colnames(x), attr(Terms, "items")$ancillary)
  colnames(variance) <- rownames(variance) <- names(res$par)

  fit <- list(coefficients = res$par, variance = variance, 
              lev = lev, terms = Terms, call = kall, 
              convergence = res$convergence, xlevels = xlev) 

  attr(fit, "na.message") <- attr(mf, "na.message") 

  if (!is.null(attr(mf, "na.action"))) 
    fit$na.action <- attr(mf, "na.action") 

  fit
}
