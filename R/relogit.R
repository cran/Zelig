relogit <- function(formula, data=sys.parent(), tau=NULL,
                    bias.correct=TRUE, ...){
  mf <- match.call()
  mf$tau <- mf$bias.correct <- NULL
  if (!is.null(tau))
    tau <- unique(tau)
  if (length(tau) > 2)
    stop("tau must be a vector of length less than or equal to 2")
  mf[[1]] <- as.name("glm")
  mf$family <- as.name("binomial")
  res <- eval(as.call(mf))
  res$call <- match.call(expand.dots = TRUE)
  ## prior correction 
  y <- res$y  
  ybar <- mean(y)
  #w1 <- tau/ybar
  #w0 <- (1-tau)/(1-ybar)
  #wi <- w1*y + w0*(1-y)
  X <- model.matrix(res)
  if (bias.correct){
    pihat <- fitted(res)
    # if (is.null(tau))
    #   W <- pihat * (1 - pihat) * wi
    # else 
    W <- pihat * (1 - pihat)
    Qdiag <- lm.influence(glm(y ~ X-1, weights=W))$hat
    #if (is.null(tau)) 
    #  xi <- 0.5 * Qdiag * ((1+w0)*pihat-w0)
    #else 
    xi <- 0.5 * Qdiag * (2*pihat - 1)
    res$coefficients <- res$coefficients -
      glm(xi ~ X - 1, weights=W)$coefficients 
  }
  if (!is.null(tau)){      
    if (tau <= 0 || tau >= 1) 
      stop("\ntau needs to be between 0 and 1.\n") 
    res$correct <- res$coefficients["(Intercept)"] - 
      log(((1-tau)/tau) * (ybar/(1-ybar)))
    names(res$correct) <- paste("tau =", round(tau, 4))
  }
  res$linear.predictors <- t(res$coef) %*% t(X) 
  res$fitted.values <- 1/(1+exp(-res$linear.predictors))
  res$zelig <- "relogit"
  class(res) <- "relogit"
  res
}

