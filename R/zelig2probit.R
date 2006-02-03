zelig2probit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$M <- mf$probit <- NULL
  mf$model <- FALSE
  mf[[1]] <- stats::glm
  mf$family <- binomial(link="probit")
  as.call(mf)
}
