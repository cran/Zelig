names.summary.vglm <- function(x, ...){
  res <- list(default = slotNames(x, ...))
  class(res) <- "names.summary.vglm"
  res[[1]]
}
