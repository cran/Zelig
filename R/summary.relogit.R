summary.relogit <- function(object, ...) {
  if (!is.numeric(object$call$tau))
    tau <- eval(object$call$tau, sys.frame(sys.parent()))
  else
    tau <- object$call$tau
  if (is.null(object$call$bias.correct))
    object$call$bias.correct <- TRUE
  dta <- eval(object$call$data, sys.parent())
  tt <- terms(object)
  dta <- dta[complete.cases(model.frame(tt, dta)),]
  n <- nrow(dta)
  k <- ncol(dta)
  bias.cor <- function(res.obj, bc, n, k){
    if (bc) {
      res.obj$cov.unscaled <- res.obj$cov.unscaled * (n/(n+k))^2
      res.obj$cov.scaled <- res.obj$cov.unscaled * res.obj$dispersion
      res.obj$coef[,2] <- sqrt(diag(res.obj$cov.scaled))
      res.obj$coef[,3] <- res.obj$coef[,1] / res.obj$coef[,2]
      res.obj$coef[,4 ] <- 2*pt(-abs(res.obj$coef[,3]), res.obj$df.residual)
    }
    res.obj
  }
  res <- summary.glm(object, ...)
  res <- bias.cor(res, object$call$bias.correct, n, k)
  res$call <- object$call
  res$correct <- object$correct
  class(res) <- "relogit"
  res
}












