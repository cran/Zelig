zelig2mloglm <- function(formula, model, data, M, ...) {
  mf$model <- mf$M <- mf$... <- NULL
  require(nnet)
  mf[[1]] <- as.name("multinom")
  mf$Hess <- TRUE
  as.call(mf)
}
