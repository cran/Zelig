names.relogit <- function(x,...){
  res <- list(default=names.default(x, ...),
            estimate = names.default(x$lower.estimate, ...))
  class(res) <- "names.relogit"
  res
}
