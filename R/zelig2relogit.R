zelig2relogit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  if (is.null(mf$bias.correct))
    mf$bias.correct <- TRUE
  mf[[1]] <- as.name("relogit")
  as.call(mf)
}
