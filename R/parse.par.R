parse.par <- function(par, x, terms) {
  k <- ncol(x)
  beta <- par[1:k]
  assign(attr(terms, "items")$coef, beta, envir = parent.frame())
  ancil.names <- attr(terms, "items")$ancillary
  j <- 0
  if (!is.null(ancil.names)) {
    j <- length(ancil.names)
    for (i in 1:j) 
      assign(ancil.names[i], par[(k + i)], envir = parent.frame())
  }
  if (length(par) > (k + j)) 
    return(par[(k + j):length(par)])
}

