qi.polr <- function(object, simpar, x, x1 = NULL, y = NULL) {
  m <- length(coef(object))
  sim.coef <- simpar[,1:m]
  sim.zeta <- simpar[,(m+1):ncol(simpar)]
  k <- length(object$zeta) + 1
  lev <- object$lev
  eta <- array()
  if (ncol(x) == 1)
    eta <- 0
  else 
    eta <- x[,-1] %*% t(sim.coef)
  ev.polr <- function(ev, eta, sim.zeta, lev) {
    for (j in 1:ncol(sim.zeta)) 
      ev[,j,] <- 1 / (1 + exp(-sim.zeta[,j] + eta))
    for (j in 0:(ncol(sim.zeta)-1)) 
      ev[,dim(ev)[2]-j,] <- ev[,dim(ev)[2]-j,]-ev[,(dim(ev)[2]-j-1),]
    ev
  }
  ev <- array(1, dim = c(nrow(sim.coef), length(lev), nrow(x)),
              dimnames = list(NULL, lev, rownames(x)))
  ev <- ev.polr(ev, eta, sim.zeta, lev)
  sim.cut <- Ipr <- array(1, dim = dim(ev), dimnames = dimnames(ev))
  pr <- matrix(NA, nrow = nrow(sim.coef), ncol = nrow(x))
  sim.cut[,1,] <- ev[,1,]
  tmp <- matrix(runif(length(sim.cut[,1,]), 0, 1), nrow =
                dim(sim.cut)[1], ncol = dim(sim.cut)[3])
  for (j in 2:length(lev))   
    sim.cut[,j,] <- sim.cut[,(j-1),] + ev[,j,]
  for (k in 1:length(lev))
    Ipr[,k,] <- as.integer(tmp > sim.cut[,k,])
  for (n in 1:dim(Ipr)[3])
    pr[,n] <- 1 + rowSums(Ipr[,,n])
  pr <- matrix(factor(pr, labels = lev, ordered = TRUE),
               nrow = nrow(sim.coef), ncol = nrow(x))
  colnames(pr) <- rownames(x)
  qi <- list(ev=ev, pr=pr)
  qi.name <- list(ev="Expected Values: P(Y=j|X)",
                  pr="Predicted Values: Y|X")
  if(!is.null(x1)){
    ev1 <- array(1, dim = c(nrow(sim.coef), length(lev), nrow(x)),
                 dimnames = list(NULL, lev, rownames(x)))
    eta1 <- x1[,-1] %*% t(sim.coef)
    ev1 <- ev.polr(ev1, eta1, sim.zeta, lev)
    qi$fd <- ev1-ev
    qi$rr <- ev1/ev
    qi.name$fd <- "First Differences: P(Y=j|X1)-P(Y=j|X)"
    qi.name$rr <- "Risk Ratio: P(Y=j|X1)-P(Y=j|X)"
  }
  if (!is.null(y)) {
    yvar <- matrix(NA, nrow = length(y), ncol = length(lev))
    tmp.ev <- tmp.pr <- array(NA, dim = dim(qi$ev))
    pr.idx <- array(NA, dim = c(nrow(pr), length(lev), nrow(x)))
    qi$ate.ev <- qi$ate.pr <- matrix(NA, dim(qi$ev)[1], dim(qi$ev)[2])
    for (i in 1:length(lev)) {
      yvar[,i] <- as.integer(y == lev[i])
      pr.idx[,i,] <- as.integer(pr[,i] == lev[i])
    }
    colnames(yvar) <- lev 
    for (j in 1:ncol(yvar)) {
      tmp.ev[,j,] <- yvar[,j] - qi$ev[,j,]
      tmp.pr[,j,] <- yvar[,j] - pr.idx[,j,]
      qi$ate.ev[,j] <- apply(tmp.ev[,j,], 1, mean)
      qi$ate.pr[,j] <- apply(tmp.pr[,j,], 1, mean)
    }
    colnames(qi$ate.ev) <- colnames(qi$ate.pr) <- lev
    qi.name$ate.ev <- "Average Treatment Effect: Y - EV"
    qi.name$ate.pr <- "Average Treatment Effect: Y - PR"
  }
  list(qi=qi, qi.name=qi.name)
}











