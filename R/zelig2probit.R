zelig2probit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- as.name("glm")
  mf$family <- as.name("binomial.probit")
  binomial.probit <<- function() binomial(link="probit")
  as.call(mf)
}
