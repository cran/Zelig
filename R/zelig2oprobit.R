zelig2oprobit <- function(formula, model, data, M, ...) {
  check <- library()
  if(any(check$results[,"Package"] == "VGAM")) 
    require(VGAM)
  else
    stop("Please install VGAM using \n     install.packages(\"VGAM\", CRAN = \"http://www.stat.auckland.ac.nz/~yee\")")
  mf <- match.call(expand.dots = TRUE)
  if(is.null(mf$equal)) 
    ordinal.probit <<- function() cumulative(link=probit, parallel=T)
  else
    ordinal.probit <<- function() cumulative(link=probit)
  mf[[1]] <- as.name("vglm")
  mf$family <- as.name("ordinal.probit")
  if (mf$M == 1)
    ndim <- length(unique(na.omit((eval(mf$formula[[2]], mf$data))))) - 1
  if (mf$M > 1) {
    ntmp <- array()
    for (i in 1:mf$M) 
      ntmp[i] <- length(unique(na.omit((eval(mf$formula[[2]], mf$data[[i]]))))) - 1
    ndim <- max(ntmp)
  }
  tmp <- cmvglm(mf$formula, mf$model, mf$equal, mf$zeros, mf$ones, ndim)
  mf$formula <- tmp$formula
  mf$constraints <- tmp$constraints
  mf$model <- mf$equal <- mf$ones <- mf$zeros <- mf$M <- NULL
  as.call(mf)
}
