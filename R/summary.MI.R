summary.MI <- function(object, subset = NULL, ...){
  res <- list()
  if(is.null(subset))
    M <- 1:length(object)
  else
    M <- c(subset)
  for (i in M)
    res[[i]] <- summary(object[[i]])
  ans <- res[[1]]
  ans$zelig <- object[[1]]$zelig
  ans$call <- object[[1]]$call
  if (length(M) > 1) {
    ans$all <- res
    coef <- se <- NULL
    for (i in M){
      if (any(class(object[[1]]) == "vglm")) {
        coef <- cbind(coef, res[[i]]@coef3[,1])
        se <- cbind(se, res[[i]]@coef3[,2])
      }
      else {
        coef <- cbind(coef, coef(res[[i]])[,1])
        se <- cbind(se, coef(res[[i]])[,2])
      }
    }
    Q <- apply(coef, 1, mean)
    U <- apply(se^2, 1, mean)
    B <- apply((coef-Q)^2, 1, sum)/(length(M)-1)
    var <- U+(1+1/length(M))*B
    nu <- (length(M)-1)*(1+U/((1+1/length(M))*B))^2
    if (any(class(object[[1]]) == "vglm")) {
      coef.table <- matrix(NA, nrow(ans@coef3), 4)
      dimnames(coef.table) <- list(c(dimnames(ans@coef3)[[1]]),
                                   c("Value", "Std. Error", "t-stat", "p-value"))
    }
    else {
      coef.table <- matrix(NA, nrow = nrow(coef(ans)), ncol = 4)
      dimnames(coef.table) <- list(rownames(coef(ans)),
                                   c("Value", "Std. Error", "t-stat", "p-value"))
    }
    coef.table[,1] <- Q
    coef.table[,2] <- sqrt(var)
    coef.table[,3] <- Q/sqrt(var)
    coef.table[,4] <- pt(abs(Q/sqrt(var)), df=nu, lower.tail=F)*2
    ans$coefficients <- coef.table
    ans$cov.scaled <- ans$cov.unscaled <- NULL
    if (!any(class(object[[1]]) == "vglm")) {
      for (i in 1:length(ans)) {
        if (is.numeric(ans[[i]]) && !names(ans)[i] %in% c("coefficients")){
          tmp <- NULL
          for (j in M) 
            tmp <- cbind(tmp, res[[j]][[pmatch(names(ans)[i], names(res[[j]]))]])
          ans[[i]] <- apply(tmp, 1, mean) 
        }
      }
    }
    class(ans) <- "summary.MI"
  }
  else
    ans <- summary(object[[M]])
  ans
}

