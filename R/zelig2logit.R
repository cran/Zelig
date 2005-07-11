zelig2logit <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- mf$robust <- NULL
  mf[[1]] <- glm
  mf$family <- binomial(link="logit")
  as.call(mf)
}
