define.par <- function(object, coef.name = "beta",
                       ancillary = NULL, start = NULL){
  mc <- match.call()
  x <- eval(as.name(attr(object, "items")$x), parent.frame())
  coef <- colnames(x)
  if (!is.null(ancillary))
    coef <- c(coef, ancillary)

  attr(object, "items")$ancillary <- ancillary
  attr(object, "items")$coef <- coef.name
  assign(as.character(mc$object), object, envir = parent.frame())
  
  if (is.null(start))
    start.val <- vector(mode = "numeric", length = length(coef))
  else
    start.val <- start

  names(start.val) <- coef
  start.val
}
