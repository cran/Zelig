zelig2poisson <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$M <- mf$robust <- NULL
  mf$model <- FALSE
  mf[[1]] <- glm
  mf$family <- poisson
  as.call(mf)
}
