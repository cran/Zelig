param.glm <- function(object, num = NULL, bootstrap = FALSE) {
  if (!bootstrap) {
    coef <- mvrnorm(num, mu=coef(object), Sigma=vcov(object))
    res <- cbind(coef, phi)
  }
  else {
    coef <- coef(object)
    res <- c(coef, phi)
  }
  res
}
