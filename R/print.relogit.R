print.relogit <-function (x, digits = max(3, getOption("digits") - 3),
                          ...) {
  if (is.null(x$call$bias.correct))
    x$call$bias.correct <- TRUE
  cat("\nCall: ", deparse(x$call), "\n\n")
    cat("Coefficients")
    if (is.character(co <- x$contrasts)) 
      cat("  [contrasts: ", apply(cbind(names(co), co), 
                                  1, paste, collapse = "="), "]")
    cat(":\n")
    print.default(format(x$coefficients, digits = digits), 
                  print.gap = 2, quote = FALSE)
    if (!is.null(x$call$tau)) {
      cat("\nIntercepts with prior correction for: \n")
      print.default(x$correct, digits = digits,
                    print.gap = 1, quote = FALSE)
    }
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ", 
        x$df.residual, "Residual\n")
    cat("Null Deviance:\t   ", format(signif(x$null.deviance, 
        digits)), "\nResidual Deviance:", format(signif(x$deviance, 
        digits)), "\tAIC:", format(signif(x$aic, digits)), "\n")
    if (x$call$bias.correct)
      cat("Rare events bias correction performed.") 
    invisible(x)
}
