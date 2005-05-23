zelig <- function(formula, model, data, by = NULL, robust = FALSE, ...) {
  fn <- paste("zelig2", model, sep = "")
  if (!exists(fn))
    stop(model, "not supported. Type help.zelig(\"models\") to list supported models.")
  mf <- match.call(expand.dots = TRUE)
  mf$robust <- NULL
  if (missing(by))
    by <- NULL
  N <- M <- 1
  object <- list()
  if (!is.data.frame(data))
    M <- length(data)
  if (M > 1)
    dat <- data[[1]]
  else
    dat <- data
  if (!is.null(by)) {
    if (any(as.character(by) %in% c(formula[[2]], formula[[3]])))
      stop("the variable selected for subsetting cannot be called in the formula.")
    idx <- dat[,by]
    mf$by <- NULL
    lev <- sort(unique(idx))
    N <- length(lev)
  }
  mf <- do.call(fn, list(formula, model, dat, N, ...))
  for (i in 1:N) {
    if (N > 1) {
      dat <- list()
      if (M > 1) {
        for (j in 1:M)
          dat[[j]] <- data[[j]][idx == lev[i],]
      }
      else
        dat <- data[idx == lev[i],]
    }
    else
      dat <- data
    obj <- list()
    for (j in 1:M) {
      if (M > 1)
        d <- dat[[j]]
      else
        d <- dat
      if (is.data.frame(d)) {
        d <- d[complete.cases(model.frame(as.formula(formula), d)),]
        mf$data <- d
        res <- eval(as.call(mf))
        res$call <- match.call()
        res$data <- res$call$data
        res$zelig <- model
        if (is.list(robust)) {
          if (any(c("lm", "glm") %in% class(res)[1])) {
            require(sandwich)
            ctmp <- class(res)
            class(res) <- c(paste(ctmp[1], ".robust", sep=""), ctmp)
            if (!any(robust$method %in% c("vcovHC", "vcovHAC", "kernHAC")))
              stop("such a robust option is not supported")
            else if ((robust$method == "vcovHC") & ("lm" != class(res)[1]))
              stop("vcovHC is supported only for ols")
            res$robust <- robust
          }
          else
            stop("robust option is not supported for this model.")
        }
        else if (robust) {
          if (any(c("lm", "glm") %in% class(res)[1])) {
            require(sandwich)
            ctmp <- class(res)
            class(res) <- c(paste(ctmp[1], ".robust", sep=""), ctmp)
          }
          else
            stop("robust option is not supported for this model.")
        }
        if (M > 1) 
          obj[[j]] <- res
        else
          obj <- res
      }
    }
    if (M > 1) 
      class(obj) <- "MI"
    if (N > 1) 
      object[[i]] <- obj
    else
      object <- obj
  }
  if (N > 1) {
    class(object) <- "strata"
    names(object) <- lev
  }
  object
}
