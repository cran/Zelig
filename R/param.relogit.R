param.relogit <- function(object, num, x, bootstrap = FALSE, bootfn = NULL, ...) {
  tau <- eval(object$call$tau, sys.parent())
  if (is.null(object$call$bias.correct))
    object$call$bias.correct <- TRUE
  if (length(tau) == 2) { # without full population information
    tmp0 <- tmp1 <- object
    tmp0$coefficients["(Intercept)"] <- object$correct[1]
    tmp1$coefficients["(Intercept)"] <- object$correct[2]
    pping <- function(object, x, x1, num, bootstrap, bootfn,...) {
      if(!bootstrap) {
        par0 <- param.default(tmp0, num=num, bootstrap=bootstrap)
        par1 <- param.default(tmp1, num=num, bootstrap=bootstrap)
      }
      else {
        dta <- eval(object$data, sys.parent())
        dta <- dta[complete.cases(model.frame(object, dta)),]
        if (is.null(bootfn)) {
          bootfn <- function(data, i, obj) {
            d <- data[i,]
            obj$call$data <- d
            fit <- eval(obj$call, sys.parent())
            fit$coefficients["(Intercept)"] <- fit$correct[1]
            coef0 <- fit$coefficients
            fit$coefficients["(Intercept)"] <- fit$correct[2]
            coef1 <- fit$coefficients
            return(c(coef0, coef1))
          }
        }
        pars <- boot(dta, bootfn, R=num, obj = object, ...)
        colnames(pars$t) <- names(pars$t0)
        par0 <- pars$t[, 1:length(object$coef)]
        par1 <- pars$t[, (length(object$coef)+1):nrow(pars$t)]
      }
      sim00 <- qi.glm(tmp0, par0, x = x)
      P00 <- as.matrix(sim00$qi$ev)
      sim10 <- qi.glm(tmp1, par1, x = x)
      P10 <- as.matrix(sim10$qi$ev)
      test <- P00[,1] < P10[,1]
      par0 <- as.matrix(par0[test,])
      par1 <- as.matrix(par1[test,])
      list(par0 = par0, par1 = par1)
    }
    tmp <- pping(object, bootstrap=bootstrap, bootfn=bootfn, x=x, x1=x1, num=num, ...)
    par0 <- tmp$par0
    par1 <- tmp$par1
    while (nrow(par0) < num) {
      tmp <- pping(object, bootstrap=bootstrap, bootfn=bootfn, x=x, x1=x1, num=num,...)
      par0 <- rbind(par0, tmp$par0)
      par1 <- rbind(par1, tmp$par1)
    }
    if (nrow(par0) > num) {
      par0 <- par0[1:num,]
      par1 <- par1[1:num,]
    }
    par0 <- as.matrix(par0)
    par1 <- as.matrix(par1)
    rownames(par0) <- 1:nrow(par0)
    rownames(par1) <- 1:nrow(par1)
    return(list(par0 = par0, par1 = par1))
  }
  else { # with precise population info, or no population info
    object$coefficients["(Intercept)"] <- object$correct
    if (!bootstrap) 
      simpar <- param.default(object, num=num, bootstrap = bootstrap)
    else {
      tt <- terms(object)
      dta <- eval(object$data, sys.parent())
      dta <- dta[complete.cases(model.frame(tt, dta)),]
      if(is.null(bootfn)) {
        bootfn <- function(data, i, object) {
          d <- data[i,]
          object$call$data <- d
          fit <- eval(object$call, sys.parent())
          fit$coefficients["(Intercept)"] <- correct
          fit$coefficients
        }
      }
      res <- boot(dta, bootfn, R = num, object = object, ...)
      colnames(res$t) <- names(res$t0)
      simpar <- res$t
    }        
    return(simpar)
  }
}
























































