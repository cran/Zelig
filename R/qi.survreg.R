qi.survreg <- function(object, simpar, x, x1 = NULL, y = NULL, cond.data = NULL) {
  model <- object$zelig
  k <- length(object$coef)
  sim.coef <- as.matrix(simpar[,1:k])
  if (model %in% c("weibull", "Weibull", "lognorm")) #{
    sim.scale <- simpar[,(k+1):ncol(simpar)]
  if (!is.null(y)) {
    status <- x[,1]
    x <- as.data.frame(x[,2:ncol(x)])
  }
  link <- survreg.distributions[[object$dist]]$itrans
  ev.pr.surv <- function(sim.coef, sim.scale, x, link) {
    eta <- sim.coef %*% t(x)
    theta <- as.matrix(apply(eta, 2, link))
    ev <- pr <- matrix(NA, ncol=ncol(theta), nrow=nrow(theta))
    dimnames(pr) <- dimnames(ev) <- dimnames(theta) 
    if (model == "exp") {
      ev <- theta
      for (i in 1:nrow(pr))
        pr[i,] <- rexp(length(ev[i,]), rate = 1/ev[i,])
    }
    else if (model == "weibull" || model == "Weibull") {
      ev <- theta * gamma(1 + exp(sim.scale))
      for (i in 1:nrow(pr))
        pr[i,] <- rweibull(length(ev[i,]), shape=1/exp(sim.scale[i]),
                           scale=theta[i,])
    }
    else if (model == "lognorm") {
      ev <- exp(log(theta) + 0.5*(1/exp(sim.scale))^2)
      for (i in 1:nrow(pr)) 
        pr[i,] <- rlnorm(length(ev[i,]), meanlog = log(theta[i,]),
                         sdlog = 1/exp(sim.scale[i]))
    }
    list(ev=ev, pr=pr)
  } 
  qi <- ev.pr.surv(sim.coef, sim.scale, x, link)
  if (!is.null(y)) {
    if (is.null(cond.data))
      stop("`cond.data' is required for the exponential, Weibull, and lognormal models.")
    call <- object$call
    call$by <- NULL
    call$data <- cond.data
    est0 <- eval(call, sys.frame())
    par0 <- matrix(param(est0, bootstrap = TRUE), nrow = 1)
    coef0 <- matrix(par0[,1:k], nrow = 1)
    if (model %in% c("weibull", "Weibull", "lognorm"))
      scale0 <- rep(par0[,(k+1):ncol(par0)], nrow(x))
    qi0 <- ev.pr.surv(coef0, scale0, x, link)
    y1 <- y
    tmp <- status
    y[status == 0] <- qi0$pr[(status == 0), 1]
    status[status == 0] <- as.integer(y1[status == 0] < y[status == 0])
    while (sum(status) < length(status)) {
      qi0 <- ev.pr.surv(coef0, scale0, x, link)
      y[status == 0] <- qi0$pr[status == 0, 1]
      status[status == 0] <- as.integer(y1[status == 0] < y[status == 0])
    }
  }
  qi.name <- list(ev="Expected Values: E(Y|X)",
                  pr="Predicted Values: Y|X")
  if (!is.null(x1)) {
    eta1.sim <- sim.coef %*% t(x1)
    ev1 <- apply(eta1.sim, 2, link)
    qi$fd <- ev1 - qi$ev
    qi.name$fd <- "First Differences: E(Y|X1)-E(Y|X)"
  }
  if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
    tmp.ev <- yvar - qi$ev
    tmp.pr <- yvar - qi$pr
    qi$ate.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    qi$ate.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(simpar))
    qi.name$ate.ev <- "Average Treatment Effect: Y - EV"
    qi.name$ate.pr <- "Average Treatment Effect: Y - PR"
  }
  list(qi=qi, qi.name=qi.name)
}  















