summarize.array <- function(x, cip, stats, model, object, subset = NULL) {
  if (is.function(subset)){  # subset = all; all is class "function"
    tmp <- summarize.default(x[,,1], cip, stats, model)
    tmp <- array(NA, dim=c(nrow(tmp), ncol(tmp), dim(x)[3]),
                 dimnames=list(dimnames(x)[[2]], dimnames(tmp)[[2]],
                   rownames(object$x)))
    for (j in 1:dim(x)[3])
      tmp[,,j] <- summarize.default(x[,,j], cip, stats, model)
    res <- tmp
  }
  if(is.null(subset)){# subset = NULL; summarizes all obs at once. 
    tmp <-NULL
    for (j in 1:dim(x)[3])
      tmp <- rbind(tmp, x[,,j])
    res <- summarize.default(tmp, cip, stats, model)
  }
  if (is.numeric(subset)) { # subset=integer, summarizes identified obs
    if (length(subset) > 1) {
      tmp <- summarize.default(x[,,1], cip, stats, model)
      tmp <- array(NA, dim=c(ncol(x), ncol(tmp), length(subset)),
                   dimnames=list(dimnames(x)[[2]],
                     dimnames(tmp)[[2]], as.character(subset))) 
      for (l in 1:length(subset))
        tmp[,,l] <- summarize.default(x[,,subset[l]], cip, stats, model)
      res <- tmp
    }
    else
      res <- summarize.default(x[,,subset], cip, stats, model)
  }
  return(res)
}
