zelig2logit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- as.name("glm")
  mf$family <- as.name("binomial")
  as.call(mf)
}
