generate.start <- function(start.val, X, ancillary = NULL) {
  if (is.null(start.val))
    start.val <- rep(0, ncol(X) + length(ancillary))
  if (!is.null(ancillary))
    names(start.val) <- c(colnames(X), ancillary)
  else
    names(start.val) <- c(colnames(X))
  start.val
}
