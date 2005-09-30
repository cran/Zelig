qi.survreg <- function(object, simpar, x, x1 = NULL, y = NULL) {
  model <- object$zelig
  k <- length(object$coef)
  sim.coef <- as.matrix(simpar[,1:k])
  if (model %in% c("weibull", "Weibull")) {
    if (ncol(simpar) == (k + 1)) 
      sim.scale <- simpar[,(k+1):ncol(simpar)]
    else
      sim.scale <- rep(object$scale, nrow(simpar))
  }
  else if (model == "lognorm")
    sim.scale <- simpar[,(k+1):ncol(simpar)]
  if (!is.null(y)) {
    status <- y[,2]
    y <- y[,1]
  }
  link <- survreg.distributions[[object$dist]]$itrans
  ev.surv <- function(model, sim.coef, sim.scale, x, link) {
    eta <- sim.coef %*% t(x)
    theta <- as.matrix(apply(eta, 2, link))
    if (model == "lognorm") {
      ev <- exp(log(theta) + 0.5*(exp(sim.scale))^2)
      dimnames(ev) <- dimnames(theta)
    }
    else if (model == "weibull" || model == "Weibull") {
      ev <- theta * gamma(1 + exp(sim.scale))
      dimnames(ev) <- dimnames(theta)
    }
    else if (model == "exp") {
      ev <- theta
    }
    list(ev = as.matrix(ev), theta = as.matrix(theta))
  }
  pr.surv <- function(model, theta, sim.scale, ev) { 
    if (model == "exp") 
      pr <- rexp(length(ev), rate = 1/ev)
    else if (model == "weibull" || model == "Weibull") 
      pr <- rweibull(length(ev), shape=1/exp(sim.scale),
                         scale=theta)
    else if (model == "lognorm") 
      pr <- rlnorm(length(ev), meanlog = log(theta),
                       sdlog = exp(sim.scale))
    pr
  }
  ev <- ev.surv(model, sim.coef, sim.scale, x, link)
  pr <- matrix(NA, ncol=ncol(ev$ev), nrow=nrow(ev$ev))
  dimnames(pr) <- dimnames(ev$ev) 
  for (i in 1:nrow(ev$ev))
    pr[i,] <- pr.surv(model, ev$theta[i,], sim.scale[i], ev$ev[i,])
  qi <- list(ev = ev$ev, pr = pr)
  qi.name <- list(ev="Expected Values: E(Y|X)",
                  pr="Predicted Values: Y|X")
  if (!is.null(x1)) {
    ev1 <- ev.surv(model, sim.coef, sim.scale, x1, link)
    qi$fd <- ev1$ev - qi$ev
    qi.name$fd <- "First Differences: E(Y|X1)-E(Y|X)"
  }
  if (!is.null(y)) {
    if (any(status == 0)) { 
      tmp <- list(ev = ev$ev[, which(status == 0)],
                  theta = ev$theta[, which(status == 0)])
      y.obs <- matrix(y[status == 1], nrow = nrow(qi$ev),
                      ncol = length(y[status == 1]), byrow = TRUE)
      y.imp <- matrix(NA, nrow = nrow(qi$ev), ncol = length(y[status == 0]))
      tmp.scale <- c(matrix(sim.scale, nrow = length(sim.scale),
                            ncol = length(y[status == 0])))
      y.imp <- matrix(pr.surv(model, tmp$theta, tmp.scale, tmp$ev),
                      nrow = nrow(qi$ev), ncol = length(y[status == 0]))
      y.c <- y[status == 0]
      idx <- t(apply(y.imp, 1, '>=', y.c))
      count <- 1
      while ((sum(idx) < length(idx)) & count < 1001) {
        count <- count + 1
        tmp.idx <- which(!idx, arr.ind = TRUE)
        y.imp[tmp.idx] <- pr.surv(model, tmp$theta[tmp.idx],
                                  sim.scale[tmp.idx[,1]], tmp$ev[tmp.idx])
        idx[tmp.idx] <- y.imp[tmp.idx] >= y.c[tmp.idx[,2]]
      }
      if (count == 1001) {
        warning("    Maximum number of imputed values (1000) reached for censored Y.  \n    Using censoring point as observed value, since Pr(Y > Yc | sims) <= 0.001.")
        y.imp[which(idx == 0, arr.ind = TRUE)] <- y.c[which(idx == 0, arr.ind == TRUE)[,2]]
      }
      yvar <- matrix(NA, ncol = length(y), nrow = nrow(qi$ev))
      yvar[, which(status == 1)] <- y.obs
      yvar[, which(status == 0)] <- y.imp
    }
    else
      yvar <- matrix(y, ncol = length(y), nrow = nrow(qi$ev), byrow = TRUE)
    tmp.ev <- yvar - qi$ev
    tmp.pr <- yvar - qi$pr
    qi$ate.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    qi$ate.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(simpar))
    qi.name$ate.ev <- "Average Treatment Effect: Y - EV"
    qi.name$ate.pr <- "Average Treatment Effect: Y - PR"
  }
  list(qi=qi, qi.name=qi.name)
}  















