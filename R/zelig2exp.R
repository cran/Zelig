zelig2exp <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  require(survival)
  mf[[1]] <- as.name("survreg")
  mf$dist <- as.character("exponential")
  as.call(mf)
}
