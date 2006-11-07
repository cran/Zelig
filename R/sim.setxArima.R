sim.setxArima <- function(object, x, x1 = NULL, num = 1000, prev = NULL,
                     bootstrap = FALSE, bootfn = NULL, cond.data = NULL,
                     max.iter=10, ...) {
  require(mvtnorm)
  if (bootstrap | !is.null(bootfn) | !is.null(cond.data)){
    warning("boostrap, bootfn, and cond.data are ignored in ARIMA models")
  }
  t.effect <- x$t.effect
  ##extracting out the series
  if (is.data.frame(object$zelig.data))
    dat <- object$zelig.data
  else
    dat <- eval(object$call$data, parent.frame())
  series <- eval(eval(object$call$formula[[2]])$name, envir=dat)
  pred.ahead <- x$pred.ahead
  if (!is.null(prev)){
    draw.parm <- prev
  }
  if (is.null(prev)){
    draw.parm <- rmvnorm(num, mean=object$coef, sigma=object$var.coef)
    ##we need to clean out the MA's and AR's that are non-stationary  
    if (object$arma[1] >= 1){
      for (i in 1:nrow(draw.parm)){
        sr <- polyroot(c(1, -draw.parm[i, (1:object$arma[1])]))
        if (any(Mod(sr)<1)){
          draw.parm[i,object$arma[1] ] <- NA
        }
      }
    }  
    if (object$arma[2] >= 1){
      for (i in 1:nrow(draw.parm)){
        sr <- polyroot(c(1, draw.parm[i, ((object$arma[1] + 1):
                                          (object$arma[1] + object$arma[2]))]))
        if (any(Mod(sr)<1)){
          draw.parm[i, (object$arma[2])] <- NA
        } 
      } 
    }  
    if (object$arma[3] >= 1){
      for (i in 1:nrow(draw.parm)){
        sr <- polyroot(c(1, -draw.parm[i, ((object$arma[1] + object$arma[2] + 1):
                                           (object$arma[1] + object$arma[2] +
                                            object$arma[3]))]))
        if (any(Mod(sr)<1)){
          draw.parm[i,(object$arma[3]) ] <- NA
        }
      }
    } 
    if (object$arma[4]>=1){
      for (i in 1:nrow(draw.parm)){
        sr <- polyroot(c(1, draw.parm[i,
                                      ((object$arma[1] + object$arma[2] +
                                        object$arma[3] + 1):
                                       (object$arma[1] + object$arma[2] +
                                        object$arma[3] + object$arma[4]))]))
        if (any(Mod(sr)<1)){
          draw.parm[i,(object$arma[4]) ] <- NA
        }
      }
    } 
    draw.parm <- na.omit(draw.parm)
    iter <-0
    while (nrow(draw.parm) < num){
      iter <- iter + 1
      if (iter <= max.iter){
        draw.new <- rmvnorm(num, mean=object$coef, sigma=object$var.coef)
        ##we need to clean out the MA's and AR's that are non-stationary  
        if (object$arma[1] >= 1){
          for (i in 1:nrow(draw.new)){
            sr <- polyroot(c(1, -draw.new[i, (1:object$arma[1])]))
            if (any(Mod(sr)<1)){
              draw.new[i,object$arma[1] ] <- NA
            }
          }
        }  
        if (object$arma[2] >= 1){
          for (i in 1:nrow(draw.new)){
            sr <- polyroot(c(1, draw.new[i, ((object$arma[1] + 1):
                                             (object$arma[1] + object$arma[2]))]))
            if (any(Mod(sr)<1)){
              draw.new[i, (object$arma[2])] <- NA
            }
          }
        } 
        if (object$arma[3] >= 1){
          for (i in 1:nrow(draw.new)){
            sr <- polyroot(c(1, -draw.new[i, ((object$arma[1] + object$arma[2] + 1):
                                              (object$arma[1] + object$arma[2] +
                                               object$arma[3]))]))
            if (any(Mod(sr) < 1)){
              draw.new[i,(object$arma[3]) ] <- NA
            }
          }
        } 
        if (object$arma[4] >= 1){
          for (i in 1:nrow(draw.new)){
            sr <- polyroot(c(1, draw.new[i, ((object$arma[1] + object$arma[2] +
                                              object$arma[3] + 1):
                                             (object$arma[1] + object$arma[2] +
                                              object$arma[3] + object$arma[4]))]))
            if(any(Mod(sr) < 1)){
              draw.new[i,(object$arma[4]) ] <- NA
            }
          }
        } 
        draw.new <- na.omit(draw.new)
        draw.parm <- rbind(draw.parm, draw.new)
      }
      if(iter > max.iter){
        warning("Number of simulations lower than desired, consider increasing maximum iterations")
      }
    }
  }
  if (nrow(draw.parm) > num) 
    draw.parm <- draw.parm[1:num,]
  if (x$min.time==1 | x$min.time==2)
    stop("Counterfactuals can only be specified from the third observation and later \n")
  if (ncol(x$dta)==0){
    ev <- matrix(NA, nrow=nrow(draw.parm), ncol=(pred.ahead))
    se <- matrix(NA, nrow=nrow(draw.parm), ncol=(pred.ahead))
    for (i in 1:nrow(draw.parm)){
      temp <- arima(series, xreg=NULL,
                    order=c(object$arma[1], object$arma[6], object$arma[2]), 
                    seasonal=list(order=c(object$arma[3], object$arma[7], object$arma[4]),
                      period=object$arma[5]), fixed=draw.parm[i,], transform.pars=TRUE)
      temp2 <-predict(temp, newxreg=NULL, n.ahead=(pred.ahead))
      ev[i,] <- temp2$pred[1:(pred.ahead)]
      se[i,] <- temp2$se[1:(pred.ahead)]	
    }
    ev <- as.matrix(ev)
    se <- as.matrix(se)
    if (!is.null(x1)){
      warning("First differences are only calculated when external regressors are used \n")
    }
  }
  if (ncol(x$dta) > 0){
    x.obs <- as.matrix(x$dta[1:(x$min.time-1),])
    x.cf <- as.matrix(x$dta[x$min.time: (x$max.time), ])
    ev <- matrix(NA, nrow=nrow(draw.parm), ncol=(length(x$min.time:x$max.time)))
    se <- matrix(NA, nrow=nrow(draw.parm), ncol=(length(x$min.time:x$max.time)))
    if (x$min.time == nrow(x$dta)){
      x.cf<- t(x.cf)
    }
    for (i in 1:nrow(draw.parm)){
      temp <- arima(series[1:(x$min.time-1)], xreg=x.obs,
                    order=c(object$arma[1], object$arma[6], object$arma[2]), 
                    seasonal=list(order=c(object$arma[3], object$arma[7], object$arma[4]),
                      period=object$arma[5]), fixed=draw.parm[i,])
      temp2 <-predict(temp, newxreg=x.cf, n.ahead=nrow(x.cf))
      ev[i,] <- temp2$pred
      se[i,] <- temp2$se
    }
    ev <- as.matrix(ev)
  }
  if (!is.null(x1)){
    if (ncol(x1$dta) > 0){
      x1.obs <- as.matrix(x1$dta[1:(x1$min.time-1), ])
      x1.cf <- as.matrix(x1$dta[x1$min.time:(x1$max.time),])
      ev.1 <- matrix(NA, nrow=nrow(draw.parm), ncol=length(x$min.time:x$max.time))
      se.1 <- matrix(NA, nrow=nrow(draw.parm), ncol=length(x$min.time:x$max.time))
      pred.ahead <- x$pred.ahead
      pred.arima.x1 <- list()
      false.arima.x1 <- list() 
      if (x1$min.time == nrow(x1$dta)){
	x1.cf <- t(x1.cf)
      }
      for (i in 1:nrow(draw.parm)){
	temp3<- arima(series[1:(x1$min.time-1)], xreg=x1.obs,
                                     order=c(object$arma[1], object$arma[6], object$arma[2]),
                                    seasonal=list(order=c(object$arma[3], object$arma[7],
                                                    object$arma[4]), period=object$arma[5]),
                                     fixed=draw.parm[i,])
	temp4 <- predict(temp3, newxreg=x1.cf, n.ahead=nrow(x.cf))
	ev.1[i,] <- temp4$pred	
	se.1[i,] <- temp4$se
      }
      ev.1 <- as.matrix(ev.1)
    }
  }
  if (!is.null(x1) & t.effect){
    warning("First differences and treatment effects are not both calculated.Calculating first differences only.\n")
  }
  if (is.null(x1) & (t.effect)){
    t.eff <- matrix(NA, nrow=nrow(ev), ncol=ncol(ev))
    for (i in 1:nrow(ev)){
      t.eff[i,] <- series[(x$min.time):x$max.time] - ev[i,]
    }
  }
  if (!is.null(x1) | !(t.effect)){
    t.eff <- NULL
  }
  if (!is.null(x1)){
    se <- as.matrix(se)
    se.1 <- as.matrix(se.1)
    fd <- ev.1 -  ev
    qi <- list(ev=ev, se=se, fd=fd, t.eff=t.eff)
    qi.name <- list(ev="Expected Values, E(Y|X)", se="Prediction Standard Error",
                    fd="First Difference, E(Y|X1) - E(Y|X)", t.eff="Treatment Effect")
    res <- list(min.time=x$min.time, qi=qi, qi.name=qi.name,
                zelig.call = object$call, 
                t.series = eval(eval(object$call$formula[[2]],
                  envir=dat)$name, envir=dat))
    class(res) <- c("zelig.arima", "zelig")
    return(res)
  } 
  if (is.null(x1)){
    se <- as.matrix(se)
    qi <- list(ev=ev, se=se, t.eff=t.eff)
    qi.name <- list(ev="Expected Values, E(Y|X)", se="Prediction Standard Error",
                    t.eff="Treatment Effect") 
    res <- list(min.time=x$min.time, qi=qi, qi.name=qi.name,
                t.series=eval(eval(object$call$formula[[2]],
                  envir=dat)$name, envir=dat),
                zelig.call = object$call)
    class(res) <- c("zelig.arima", "zelig")
    return(res) 
  } 
} 