summarize.default <- function(x, cip, stats, model, object, subset = NULL) {

  res <- NULL
  if (!is.matrix(x))
    x <- as.matrix(x)
  if (is.numeric(x)) {
    if (!is.null(stats))
      for (i in 1:length(stats)) 
        res <- cbind(res, apply(x, 2, stats[i]))
    res <- cbind(res, apply(x, 2, quantile, prob=cip[1]))
    res <- cbind(res, apply(x, 2, quantile, prob=cip[2]))
    colnames(res) <- c(stats, paste(cip[1]*100, "%", sep=""),
                       paste(cip[2]*100, "%", sep=""))
  }
  else {
    res <- t(apply(if(is.matrix(x)) x else as.matrix(x), 2, table))
    if (length(unique(x))==1) {
        colnames(res) <- unique(x)
        if(model=="relogit")
          if(colnames(res)==1) {
            res <- cbind(0, res)
            colnames(res)[1] <-0
          }
          else {
            res <- cbind(res, 0)
            colnames(res)[2] <-1
          }
      }
    res <- res/nrow(x) * 100
  }
  return(res)
}
