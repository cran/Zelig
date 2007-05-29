zelig2logit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$M <- mf$robust <- NULL
  mf$model <- FALSE
  mf[[1]] <- stats::glm
  mf$family <- binomial(link="logit")
  as.call(mf)
}
