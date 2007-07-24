zelig2ologit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- MASS::polr
  mf$Hess <- TRUE
  mf$method <- "logistic"
  as.call(mf)
}
