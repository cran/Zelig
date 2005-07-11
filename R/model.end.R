model.end <- function(res, D) {

  res$variance <- -solve(res$hessian)
  res$hessian <- NULL

  colnames(res$variance) <- rownames(res$variance) <- names(res$par)
  res$coefficients <- res$par
  res$par <- NULL

  res$terms <- attr(D, "terms")

  attr(res, "na.message") <- attr(D, "na.message") 
  if (!is.null(attr(D, "na.action"))) 
    res$na.action <- attr(D, "na.action") 

  res
}
