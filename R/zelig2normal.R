zelig2normal <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- mf$robust <- NULL
  mf[[1]] <- glm
  mf$family <- gaussian
  as.call(mf)
}
