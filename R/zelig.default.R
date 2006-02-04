zelig.default <- function(formula, model, data, by = NULL, save.data =
                          FALSE, ...) {
  fn1 <- paste("zelig2", model, sep = "")
  fn2 <- paste("zelig3", model, sep = "")
  if (!exists(fn1))
    stop(model, "not supported. Type help.zelig(\"models\") to list supported models.")
  mf <- zelig.call <- match.call(expand.dots = TRUE)
  zelig.call[[1]] <- as.name("zelig")
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
  mf <- do.call(fn1, list(formula, model, dat, N, ...))
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
        d <- d[complete.cases(model.frame(mf$formula, data=d,
                                          na.action = na.pass)),]
        mf$data <- d
        res <- eval(as.call(mf))
        if (exists(fn2)) 
          res <- do.call(fn2, list(res = res, fcall = mf,
                                   zcall = as.list(zelig.call)))
        res$call <- zelig.call
        if (save.data)
          res$data <- d
        else
          res$data <- res$call$data
        res$zelig <- model
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
