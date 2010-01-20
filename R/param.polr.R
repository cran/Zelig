param.polr <- function(object, num, bootstrap=FALSE) {
  num <- 100
  coef <- object$coefficients
  zeta <- object$zeta
  k <- length(coef)
  if (!bootstrap) {
    theta <- zeta
    res <- matrix(mvrnorm(num, mu=c(coef,theta), Sigma=vcov(object)),nr=num)
  }
  else
    res <- c(coef, zeta)
  res
}
