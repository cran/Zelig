qi.MCMCZelig <- function(object, simpar=NULL, x, x1 = NULL, y = NULL, ...) {

  model <- object$zelig
  qi <- list()
  check <- FALSE
  if (model %in% c("MCMClogit", "MCMCprobit")) 
    check <- TRUE
  
  if (model %in% c("MCMClogit","MCMCprobit", "MCMCregress", "MCMCpoisson")) {

    if (model == "MCMClogit") {
      coef <- object$coefficients
      eta <- coef %*% t(x)
      pr <- ev <- matrix(NA, nrow = nrow(eta), ncol = ncol(eta))
      dimnames(pr) <- dimnames(ev) <- dimnames(eta)
      ev <- 1/(1+exp(-eta))
      for (i in 1:ncol(ev)) 
        pr[,i] <- as.character(rbinom(length(ev[,i]), 1, ev[,i])) 
      qi$ev <- ev
      qi$pr <- pr
      qi.name <- list(ev = "Expected Values: E(Y|X)", pr="Predicted
      Values: Y|X")
    }
    else if (model == "MCMCprobit") {
      coef <- object$coefficients
      eta <- coef %*% t(x)
      pr <- ev <- matrix(NA, nrow = nrow(eta), ncol = ncol(eta))
      dimnames(pr) <- dimnames(ev) <- dimnames(eta)
      ev <- pnorm(eta)
      for (i in 1:ncol(ev)) 
        pr[,i] <- as.character(rbinom(length(ev[,i]), 1, ev[,i]))
      qi$ev <- ev
      qi$pr <- pr
      qi.name <- list(ev = "Expected Values: E(Y|X)", pr="Predicted
      Values: Y|X")
    }
    else if (model =="MCMCregress") {
      coef <- object$coefficients[,1:(ncol(object$coefficients)-1)]
      eta <- coef %*% t(x)
      pr <- ev <- matrix(NA, nrow = nrow(eta), ncol = ncol(eta))
      dimnames(pr) <- dimnames(ev) <- dimnames(eta)
      ev <- eta
    qi$ev <- ev
    qi.name <- list(ev = "Expected Values: E(Y|X)")
    }
    else if (model == "MCMCpoisson") {
      coef <- object$coefficients
      eta <- coef %*% t(x)
      pr <- ev <- matrix(NA, nrow = nrow(eta), ncol = ncol(eta))
      dimnames(pr) <- dimnames(ev) <- dimnames(eta)
      ev <- exp(eta)
      for (i in 1:ncol(ev)) 
        pr[,i] <- rpois(length(ev[,i]), ev[,i])
      qi$ev <- ev
      qi$pr <- pr
      qi.name <- list(ev = "Expected Values: E(Y|X)", pr="Predicted
      Values: Y|X")

    }
    

        
    if (!is.null(x1)) {
        eta1 <- coef %*% t(x1)
      if (model == "MCMClogit") {
        ev1 <- 1/(1+exp(-eta1))
        rr <-ev1/ev
        fd <-ev1-ev

        qi$fd <- fd
        qi$rr <- rr

        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"

        qi.name$rr <- "Risk Ratios: P(Y=1|X1)/P(Y=1|X)"
      }
      else if (model == "MCMCprobit") {
        ev1 <- pnorm(eta1)
                rr <-ev1/ev
        fd <-ev1-ev

        qi$fd <- fd
        qi$rr <- rr

        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"

        qi.name$rr <- "Risk Ratios: P(Y=1|X1)/P(Y=1|X)"
      }
      else if (model == "MCMCregress") {
        ev1 <- eta1
        fd <-ev1-ev

        qi$fd <- fd

        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"
      }
      else if (model == "MCMCpoisson") {
        ev1 <- exp(eta1)
        fd <-ev1-ev

        qi$fd <- fd

        qi.name$fd <- "First Differences in Expected Values: E(Y|X1)-E(Y|X)"

      }
   }

    if (!is.null(y)) {
    yvar <- matrix(rep(y, nrow(simpar)), nrow = nrow(simpar), byrow = TRUE)
    tmp.ev <- yvar - qi$ev
    if (check) 
      tmp.pr <- yvar - as.integer(qi$pr)
   else
      tmp.pr <- yvar - qi$pr
    qi$ate.ev <- matrix(apply(tmp.ev, 1, mean), nrow = nrow(simpar))
    qi.name$ate.ev <- "Average Treatment Effect: Y - EV"
    if (model %in% c("MCMClogit", "MCMCprobit", "MCMCpoisson"))
      {
        qi$ate.pr <- matrix(apply(tmp.pr, 1, mean), nrow = nrow(simpar))
        qi.name$ate.pr <- "Average Treatment Effect: Y - PR"
      }
  }

    list(qi=qi, qi.name=qi.name)    
}
  
  else if (model == "MCMChierEI") {
    if (!any(class(x)=="cond")) stop("set 'cond=TRUE' in setx.\n")
    else
      {
        coef <- object$coefficients
        n <- nrow(x)
        if (is.null(object$N))
          N<-rep(1,n)
        else N <- eval(object$N)
        ev <- array(NA, c(nrow = nrow(coef), 2,2, n))
        pr <- array(NA, c(nrow = nrow(coef), 2,2, n))
        nlen<-length(coef[,1])
        for (j in 1:2) {
          ev[,j,1,] <- t(apply(coef[,((1:n)+(j-1)*n)],
                               1,"*",  x[,j])*N)
          ev[,j,2,] <- t(apply((1-coef[,((1:n)+(j-1)*n)]), 1,"*",
                               x[,j])*N)
          for (i in 1:n)
            {
              size<-round(x[i,j]*N[i])
              pr[,j,1,i] <-rbinom(prob=coef[,(i+(j-1)*n)],  n=nlen, size=size)

              pr[,j,2,i] <- x[i,j]*N[i]-pr[,j,1,i]
            }
          
        }
        
        dimnames(ev)[[4]] <- dimnames(pr)[[4]] <- rownames(x)
        dimnames(ev)[[2]] <- dimnames(pr)[[2]] <- colnames(x)
        dimnames(ev)[[3]] <- dimnames(pr)[[3]] <- colnames(model.response(object$model))
        class(ev) <- class(pr) <- c("ei", "array")    
        qi$ev <- ev
        qi$pr <- pr
        qi.name <- list(ev = "Expected In sample predictions at aggregate
    level", pr = "In sample predictions at aggregate level")
        
      }
    
    list(qi=qi, qi.name=qi.name)
  }
}
  
  
  



