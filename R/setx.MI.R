setx.MI <- function(object, fn = list(numeric = mean, ordered =
                              median, other = mode), data = NULL,
                    cond = FALSE, counter = NULL, ...) {
  M <- length(object)
  dta <- NULL
  if (!cond) {# unconditional predition
    obj <- object[[1]]
    for (i in 1:M) {
      if(is.null(data))
        tmp <- as.data.frame(eval(object[[1]]$call$data,
                              sys.parent())[[i]])
      else
        tmp <- data[[i]]
      dta <- rbind(dta, tmp)
    }
    X <- setx(object[[1]], fn = fn, data = dta, cond = FALSE,
              counter = NULL, ...)
    class(X) <- c("setx.MI", "setx")
  }
  else { # conditional prediction
    X <- list()
    if (is.null(data))
      data <- eval(object[[1]]$call$data, sys.parent())
    for (i in 1:M)
      X[[i]] <- setx(object[[i]], fn = NULL, data = data[[i]], cond = TRUE,
                              counter = counter, ...)
    class(X) <- c("setx.MI", "cond")
  }
  return(X)
}

