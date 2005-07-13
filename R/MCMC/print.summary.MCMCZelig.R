print.summary.MCMCZelig <- function(object, digits=max(3, getOption("digits") - 3), ...) {
  cat("\nCall: ") 
  print(object$call) 
  cat("\n", "Iterations = ", object$start, ":", object$end, "\n", sep = "")
  cat("Thinning interval =", object$thin, "\n")
  cat("Number of chains =", object$nchain, "\n")
  cat("Sample size per chain =", (object$end -
  object$start)/object$thin + 1, "\n")
  cat("\n", "Mean, standard deviation, and quantiles for marginal posterior distributions.", "\n")
  print.matrix(round(object$summary, digits=digits))
  cat("\n")
}
