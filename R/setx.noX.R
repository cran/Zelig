setx.noX <- function(object, data = NULL, fn = NULL, cond = TRUE, ...) {
  if (!is.null(fn) || !cond)
    stop(paste("\n", object$call$model, "is only appropriate in a CONDITIONAL prediction research design!"))
  if (any(class(object) == "latent"))  
    x <- eval(object$call$formula[[2]])
  else if (any(class(object) == "EI")) {
    if (is.null(data))
      data <- eval(object$call$data, sys.parent())
    x <- model.frame(object, data = data, ...)
    x1 <- x[[1]]
    for (i in 2:length(x))
      x1 <- cbind(x[[1]], x[[i]])
    x1 <- data.frame(x1)
    if (is.null(object$call$covar)) 
      names(x1) <- c(colnames(x[[1]]), colnames(x[[2]]))
    else
      names(x1) <- c(colnames(x[[1]]), colnames(x[[2]]),
                    deparse(object$call$covar[[3]]))
    rownames(x1) <- rownames(x)
    x <- x1
  }
  class(x) <- c("cond", "data.frame")
  x
}
