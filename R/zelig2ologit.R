zelig2ologit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  mf[[1]] <- as.name("polr")
  mf$Hess <- TRUE
  as.call(mf)
}
