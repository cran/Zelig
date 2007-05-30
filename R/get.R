getzelig <- function(x) {
  check <- slotNames(x)
  if (length(check) > 0) return(x@call$model)
  else return(x$call$model)
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

getdata <- function(x) {
  check <- slotNames(x)
  if (length(check) > 0) {
    if ("data" %in% check) return(x@data)
    else if ("model" %in% check) return(x@model)
    else return(NULL)
  }
  else return(x$zelig.data)
}
