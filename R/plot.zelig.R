plot.zelig <- function(x, xlab = "", user.par = FALSE, ...) {
  if (dim(x$x)[1] > 1) 
      plot.ci(x, xlab = "", ...)
  else {
    class(x) <- x$zelig.call$model
    if (exists(paste("plot.zelig", x$zelig.call$model, sep = ".")))
      UseMethod("plot.zelig", x)
    else
      stop(paste(x$zelig.call$model, "does not have an applicable plot method for sim() output."))
  }
}
