print.relogit <- function(x, digits = max(3, getOption("digits") - 3),
                          ...) {
  print.glm(x)
  if (x$prior.correct) 
    cat("\nPrior correction performed with tau =", x$tau, "\n")
  if (x$weighting) 
    cat("\nWeighting performed with tau =", x$tau, "\n")
  if (x$bias.correct)
    cat("Rare events bias correction performed\n") 
  invisible(x)
}
