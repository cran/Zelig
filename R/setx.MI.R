setx.MI <- function(object, fn = list(numeric = mean, ordered =
                              median, other = mode), data = NULL,
                    cond = FALSE, counter = NULL, ...) {
  M <- length(object)
  dta <- NULL
  obj <- object[[1]]
  if (!cond) {# unconditional predition
    for (i in 1:M) {
      if(is.null(data))
        tmp <- as.data.frame(eval(getcall(obj)$data,
                              sys.parent())[[i]])
      else
        tmp <- data[[i]]
      dta <- rbind(dta, tmp)
    }
    #X <- NextMethod("setx", object = object[[1]], fn = fn, data = dta, cond = FALSE,
    #                counter = NULL, ...)
    X <- setx(object[[1]], fn = fn, data = dta, cond = FALSE,
             counter = NULL, ...)
    class(X) <- c("setx.MI", "setx", "data.frame")
  }
  else { # conditional prediction
    X <- list()
    if (is.null(data))
      data <- eval(getcall(obj)$data, sys.parent())
    for (i in 1:M){
      X[[i]] <- setx(object[[i]], fn = NULL, data = data[[i]], cond = TRUE,
                              counter = counter, ...)
	class(X[[i]]) <- c("cond", "data.frame")
	}
    class(X) <- c("setx.MI", "setx.cond", "cond")
  }
  return(X)
}

