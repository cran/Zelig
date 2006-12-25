getzelig <- function(x) {
  check <- slotNames(x)
  if (length(check) > 0) return(x@zelig)
  else return(x$zelig)
}

getcall <- function(x) {
  check <- slotNames(x)
  if (length(check) > 0) return(x@call)
  else return(x$call)
}

getcoef <- function(x) {
  check <- slotNames(x)
  if (length(check) > 0) {
    if ("coef3" %in% check) return(x@coef3)
    else return(x@coef)
  }
  else return(x$coef)
}

