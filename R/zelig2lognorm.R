zelig2lognorm <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  require(survival)
  mf[[1]] <- as.name("survreg")
  mf$dist <- as.character("lognormal")
  as.call(mf)
}
