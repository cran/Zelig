describe.bivariate.probit <- function() {
  category <- "bivaraite.dichotomous"
  package <- list(name = "mvtnorm", 
                  version = "0.7")
  mu <- list(equations = c(2,2),               # Systematic component 
             tagsAllowed = TRUE,          
             depVar = TRUE, 
             expVar = TRUE)
  rho <- list(equations = c(1,1),              # Optional systematic component
             tagsAllowed = FALSE,         #   Estimated as an ancillary
             depVar = FALSE,              #   parameter by default
             expVar = TRUE)
  pars <- list(mu = mu, rho = rho)
  list(category = category, parameters = pars)
}

zelig2bivariate.probit <- function(formula, model, data, M, ...) {
  require(mvtnorm)
  mf <- match.call(expand.dots = TRUE)
  mf$formula <- parse.formula(formula, model)
  mf$model <- mf$M <- NULL
  mf[[1]] <- as.name("bivariate.probit")
  as.call(mf)
}

bivariate.probit <- function(formula, data, start.val = NULL, ...) {
  
  fml <- formula
 D <- model.frame(fml, data = data)
  X <- model.matrix(fml, data = D, eqn = c("mu1", "mu2"))       # [1]
  Xrho <- model.matrix(fml, data = D, eqn = "rho")

  Y <- model.response(D)
  terms <- attr(D,"terms")
  start.val <- set.start(start.val, terms)
  start.val <- put.start(start.val, 1, terms, eqn = "rho")

  log.lik <- function(par, X, Y, terms) {
    Beta <- parse.par(par, terms, eqn = c("mu1", "mu2"))         # [2]
    gamma <- parse.par(par, terms, eqn = "rho")
    rho <- (exp(Xrho %*% gamma) - 1) / (1 + exp(Xrho %*% gamma))

    mu <- X %*% Beta                                             # [3]
    llik <- 0
    for (i in 1:nrow(mu)){
          Sigma <- matrix(c(1, rho[i,], rho[i,], 1), 2, 2)
      if (Y[i,1]==1)
        if (Y[i,2]==1)
          llik <- llik + log(pmvnorm(lower = c(0, 0), upper = c(Inf, Inf), 
                                     mean = mu[i,], corr = Sigma))
        else
          llik <- llik + log(pmvnorm(lower = c(0, -Inf), upper = c(Inf, 0), 
                                     mean = mu[i,], corr = Sigma))
      else
        if (Y[i,2]==1)
          llik <- llik + log(pmvnorm(lower = c(-Inf, 0), upper = c(0, Inf),
                                     mean = mu[i,], corr = Sigma))
        else
          llik <- llik + log(pmvnorm(lower = c(-Inf, -Inf), upper = c(0, 0), 
                                     mean = mu[i,], corr = Sigma))
        }
    return(llik)
  }

  res <- optim(start.val, log.lik, method = "BFGS",
               hessian = TRUE, control = list(fnscale = -1),
               X = X, Y = Y, terms = terms, ...)

  fit <- model.end(res, D)
  class(fit) <- "bivariate.probit"
  fit
}

data(sanction)
user.prompt()
z.out1 <- zelig(cbind(import, export) ~ coop + cost + target, model = "bivariate.probit", data = sanction)
user.prompt()
z.out2 <- zelig(list(mu1 = import ~ coop, mu2 = export ~ cost + target),
                  model = "bivariate.probit", data = sanction)
user.prompt()

