param.polr <- function(object, num, bootstrap=FALSE) {
  coef <- object$coefficients
  zeta <- object$zeta
  k <- length(coef)
  if (!bootstrap) {
    par <- mvrnorm(num, mu=c(coef,zeta), Sigma=vcov(object))
    coef <- par[,1:k]
    alpha <- par[,(k+1):ncol(par)]
    res <- as.matrix(cbind(coef, alpha))
  }
  else
    res <- c(coef, zeta)
  res
}
