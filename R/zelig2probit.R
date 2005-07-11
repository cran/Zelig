zelig2probit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- mf$probit <- NULL
  mf[[1]] <- stats::glm
  mf$family <- binomial(link="probit")
  as.call(mf)
}
