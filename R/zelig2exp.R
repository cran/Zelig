zelig2exp <- function(formula, model, data, M, ...) {
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
  require(survival)
  mf[[1]] <- survival::survreg
  mf$dist <- "exponential"
  if (is.null(mf$robust))
    mf$robust <- FALSE
  if (!is.null(mf$cluster) & !mf$robust) 
    stop("\nIf cluster is specified, robust must be TRUE.")
  if (!is.null(mf$cluster)) {
    mf$formula <- as.formula(paste(paste(deparse(formula[[2]])),
                                   paste(deparse(formula[[1]])),
                                   paste(deparse(formula[[3]], width.cutoff=500)),
                                   paste("+", " cluster(",
                                         mf$cluster, ")")))
    mf$cluster <- NULL
  }
  else if (mf$robust) 
    mf$formula <- as.formula(paste(paste(deparse(formula[[2]])),
                                   paste(deparse(formula[[1]])),
                                   paste(deparse(formula[[3]], width.cutoff=500)),
                                   paste("+", " cluster(1:nrow(",
                                         deparse(formula[[2]]),"))")))
  as.call(mf)
}
