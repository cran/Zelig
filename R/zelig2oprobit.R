zelig2oprobit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- MASS::polr
  mf$Hess <- TRUE
  mf$method <- "probit"
  as.call(mf)
}
