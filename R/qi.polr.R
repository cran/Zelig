qi.polr <- function(object, simpar, x, x1 = NULL, y = NULL) {
  num <- nrow(simpar)
  m <- length(coef(object))
  sim.coef <- simpar[,1:m]
  sim.zeta <- simpar[,(m+1):ncol(simpar)]
  k <- length(object$zeta) + 1
  lev <- object$lev
  eta <- t(x[,-1] %*% t(sim.coef)) 
  Ipr <- cuts <- tmp0 <- array(0, dim = c(num, k, nrow(x)),
                        dimnames = list(1:num, lev, rownames(x)))
  for (i in 1:num) 
    cuts[i,,] <- t(object$inv.link(eta[i,], sim.zeta[i,]))
  tmp0[,(2:k),] <- cuts[,(1:(k-1)),]
  ev <- cuts - tmp0
  pr <- matrix(NA, nrow = num, ncol = nrow(x))
  tmp <- matrix(runif(length(cuts[,1,]), 0, 1),
                nrow = num , ncol = nrow(x))
  for (i in 1:k)
    Ipr[,i,] <- as.integer(tmp > cuts[,i,])
  for (n in 1:nrow(x))
    pr[,n] <- 1 + rowSums(Ipr[,,n])
  pr <- matrix(factor(pr, labels = lev, ordered = TRUE),
               nrow = nrow(sim.coef), ncol = nrow(x))
  colnames(pr) <- rownames(x)
  qi <- list(ev = ev, pr = pr)
  qi.name <- list(ev = "Expected Values: P(Y=j|X)",
                  pr = "Predicted Values: Y|X")
  if(!is.null(x1)){
    eta1 <- t(x1[,-1] %*% t(sim.coef))
    Ipr <- cuts <- tmp0 <- array(0, dim = c(num, k, nrow(x)),
                                 dimnames = list(1:num, lev, rownames(x)))
    for (i in 1:num) 
      cuts[i,,] <- t(object$inv.link(eta1[i,], sim.zeta[i,]))
    tmp0[,(2:k),] <- cuts[,(1:(k-1)),]
    ev1 <- cuts - tmp0
    qi$fd <- ev1 - ev
    qi$rr <- ev1 / ev
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











