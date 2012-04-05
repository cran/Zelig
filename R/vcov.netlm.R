vcov.netlm <- function(object, ...) {
  so <- summary.lm(object, correlation = FALSE, ...)
  so$sigma^2 * so$cov.unscaled
}
