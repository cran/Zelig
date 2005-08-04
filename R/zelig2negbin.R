zelig2negbin <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- mf$robust <- NULL
  mf[[1]] <- MASS::glm.nb
  as.call(mf)
}
