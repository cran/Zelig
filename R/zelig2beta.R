zelig2beta <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- as.name("beta.regression")
  as.call(mf)
}
