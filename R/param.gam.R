param.gam <- function(object, num = NULL, bootstrap = FALSE) {
  if (!bootstrap) {
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
    if (getzelig(object) == "gam.normal") {
      df <- object$df.residual
      sig2 <- summary(object)$dispersion
      alpha <- sqrt(df*sig2/rchisq(num, df=df))
      res <- cbind(coef, alpha)
    }
    else if (getzelig(object) == "gam.gamma")  {
      rate <- gamma.shape(object) 
      alpha <- rnorm(num, mean = rate$alpha, sd = rate$SE)
      res <- cbind(coef, alpha)
    }
    else if (getzelig(object) == "gam.negbin") {
      alpha <- object$theta
      res <- cbind(coef, c(alpha))
    }
    else
      res <- coef
  }
  res
}




