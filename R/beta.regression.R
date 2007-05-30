beta.regression <- function(formula, data, start = NULL, ...) {
  kall <- match.call()
  Terms <- define.data(formula, data)
  if (is.null(start))
    start <- c(lm(y ~ x - 1)$coef, 1)
  start.val <- define.par(Terms, coef.name = "beta", ancillary = "phi",
                          start = start)
  llik.beta <- function(par, X, Y, Terms) {
    parse.par(par, X, Terms)
    Xbeta <- X %*% beta
    mu <- exp(Xbeta) / (1 + exp(Xbeta))
    sum(lgamma(phi) - lgamma(mu * phi) - lgamma((1 - mu) * phi) +
        (mu * phi - 1) * log(Y) + ((1 - mu) * phi - 1) * log(1 - y))
  }
  res <- optim(start.val, llik.beta, method = "BFGS",
               control = list(fnscale = -1),
               hessian = TRUE, X = x, Y = y,
               Terms = Terms, ...)
  fit <- model.end(Terms, res)
  class(fit) <- "BetaReg"
  fit
}
