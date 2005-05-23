summary.zelig<-function(object, subset = NULL, CI=95, 
                        stats=c("mean", "sd"), ...){
  cip <- c((100-CI)/200, 1-(100-CI)/200)
  qi.stats <- list()
  X <- object$x
  X1 <- object$x1
  if (any(class(X)=="setx.MI") & any(class(X)=="cond")) {
    X <- NULL
    for (i in 1:length(object$x)) 
      X <- rbind(X, object$x[[i]])
  }
  if (any(class(X1)=="setx.MI")) {
    X1 <- NULL
    for (i in 1:length(object$x1)) 
      X1 <- rbind(X1, object$x1[[i]])
  }
  if (is.null(dim(X))) {
    X <- matrix(X, ncol = 1)
    colnames(X) <- "(Intercept)"
    if (!is.null(X1)) {
      X1 <- matrix(X1, ncol = 1)
      colnames(X1) <- "(Intercept)"
    }
  }
  if (is.numeric(subset)) {
    X <- X[subset,]
    if (!is.null(X1))
      X1 <- X1[subset,]
  }
  rows <- rownames(X)
  for (i in 1:length(object$qi)) {
    qi.stats[[i]] <- summarize(object$qi[[i]], rows = rows,
                               stats = stats, cip = cip, 
                               subset = subset)
    if (is.matrix(qi.stats[[i]]))
      qi.stats[[i]] <- t(qi.stats[[i]])
    if (is.table(qi.stats[[i]]))
      qi.stats[[i]] <- t(as.matrix(qi.stats[[i]]))
    if (is.null(subset) && nrow(X) > 1)
      object$qi.name[i] <- paste("Pooled", object$qi.name[[i]])
  }
  names(qi.stats) <- names(object$qi)
  res <- list(model=object$zelig$model, num=object$call$num, x=X,
              x1=X1, qi.stats=qi.stats, qi.name=object$qi.name) 
  class(res) <- "summary.zelig"
  return(res)
}




















