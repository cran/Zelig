zelig2gamma <- function(formula, model, data, M, ...) {
  mf <-  match.call(expand.dots = TRUE)
  mf$model <- mf$M <- mf$robust <- NULL
  mf[[1]] <- glm
  mf$family <- Gamma
  as.call(mf)
}
