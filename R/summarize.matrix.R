summarize.matrix <- function(x, cip, stats, model, object, subset = NULL) {
  res <- NULL
  if (dim(x)[2]==1)
    res <- summarize.default(x, cip, stats, model)
  else {
    if (is.function(subset)) 
      res <- summarize.default(x, cip, stats, model)
    if (is.null(subset)) {
      tmp <- matrix(c(x), ncol=1)
      tmp <- summarize.default(tmp, cip, stats, model)
      res <- as.matrix(tmp)
      names(res) <- colnames(tmp)
    }
    if (is.numeric(subset)) {
      res <- summarize.default(as.matrix(x[,subset]), cip, stats, model)
      rownames(res) <- subset
    }
  }
  return(res)
}
