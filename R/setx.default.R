setx.default <- function(object, fn = list(numeric = mean, ordered =
                                   median, other = mode), data = NULL,
                         cond = FALSE, counter = NULL, ...){
  mf <- match.call()
  mode <- function(x){
    tb <- tapply(x, x, length)
    if(is.factor(x))
      value <- factor(unlist(labels(tb[seq(along=tb)[tb==max(tb)]])),
                      levels=levels(x))
    else if (is.logical(x))
      value <- as.logical(unlist(labels(tb[seq(along=tb)[tb==max(tb)]])))
    else if (is.character(x))
      value <- as.character(unlist(labels(tb[seq(along=tb)[tb==max(tb)]])))
    else
      stop(paste(vars[i], "is not a supported variable type."))
    if (length(value)>1) {
      warning("There is more than one mode. The first level is selected.")
      value <- sort(value)[1]
    }
    return(value)
  }
  median.default <- median
  median <- function(x) {
    if(is.numeric(x))
      value <- median.default(x)
    else if (is.ordered(x))
      value <- factor(levels(x)[median.default(as.integer(x))],
                      levels=levels(x)) 
    else
      stop("median cannot be calculated for this data type")
    return(value)
  }
  max.default <- max
  max <- function(x, na.rm=FALSE) {
    if(is.numeric(x))
      value <- max.default(x, na.rm=na.rm)
    else if (is.ordered(x)) 
      value <- factor(levels(x)[length(levels(x))], levels=levels(x))
    else
      stop("max cannot be calculated for this data type")
    return(value)
  }
  min.default <- min
  min <- function(x, na.rm=FALSE) {
    if(is.numeric(x))
      value <- min.default(x, na.rm = na.rm)
    else if (is.ordered(x))
      value <- factor(levels(x)[1], levels=levels(x))
    else
      stop("min cannot be calculated for this data type")
    return(value)
  }
  if (any(class(object)=="vglm"))
    tt <- object@terms$terms
  else
    tt <- terms(object)
  if (is.null(data)) 
    dta <- eval(object$call$data, sys.parent())
  else
    dta <- data
  data <- dta <- dta[complete.cases(model.frame(tt, dta)), ]
  vars <- names(dta)
  if (!is.null(counter)) {
    if (!any(counter == names(model.frame(tt, dta))))
      stop(paste("the variable specified for counter is not used in the model"))
    treat <- data[, names(data)==counter]
    if(is.numeric(treat)) {
      data[treat==1, names(data)==counter] <- 0
      data[treat==0, names(data)==counter] <- 1
    }
    else if(is.factor(treat)) {
      lev <- levels(treat)
      if(length(lev)==2) {
        treat <- as.numeric(treat) - 1 
        data[treat==1, names(data)==counter] <- lev[1]
        data[treat==0, names(data)==counter] <- lev[2]
      }
      else
        stop(paste("counter only takes a binary variable"))
    }
    else if(is.logical(treat)) {
      treat <- as.numeric(treat)
      data[treat==1, names(data)==counter] <- FALSE
      data[treat==0, names(data)==counter] <- TRUE
    }
    else
      stop(paste("not supported variable type for counter"))
    if(!cond)
      stop(paste("if counter is specified, cond must be TRUE"))
  }
  if (cond) {
    if (is.null(data)) 
      stop(paste("if cond = TRUE, you must specify the data frame."))
    if (!is.null(fn)) {
      warning(paste("when cond = TRUE, fn is coerced to NULL"))
      fn <- NULL
    }
    if (class(object)[1] == "vglm")
      Y <- model.response(model.frame(tt, data=dta), "any")
    else
      Y <- model.extract(model.frame(tt, data=dta), "response")
  }
  else  if (!is.null(fn)) {
    if (is.null(fn$numeric) || !is.function(fn$numeric)) {
      warning("fn$numeric coerced to mean().")
      fn$numeric <- mean
    }
    if (is.null(fn$ordered) || !is.function(fn$ordered) || 
        identical(mean, fn$ordered)) {
      warning("fn$ordered coreced to median().")
      fn$ordered <- median
    }
    else if (identical(min.default, fn$ordered)) 
      fn$ordered <- min
    else if (identical(max.default, fn$ordered)) 
      fn$ordered <- max
    else if (identical(median.default, fn$ordered)) 
      fn$ordered <- median
    if (is.null(fn$other) || !is.function(fn$other)) { 
      warning("the only available fn for other is mode.")
      fn$other <- mode
    }
    for (i in 1:ncol(dta)) {
      v <- na.omit(dta[,i])
      if (any(vars[i] == names(model.frame(tt, dta)))) {
        if (is.numeric(v))
          value <- lapply(list(v), fn$numeric)[[1]]
        else if (is.ordered(v)) 
          value <- lapply(list(v), fn$ordered)[[1]]
        else 
          value <- lapply(list(v), fn$other)[[1]]
        data[,i] <- value
      }
    }
    opt <- vars[na.omit(pmatch(names(mf), vars))]
    maxl <- 1
    if (length(opt) > 0)
      for (i in 1:length(opt)) {
        value <- eval(mf[[opt[i]]], sys.parent())
        lv <- length(value)
        if (lv>1)
          if (maxl==1 || maxl==lv) {
            maxl <- lv
            data <- data[1:lv,]
          }
          else
            stop("vector inputs should have the same length.")
        if (is.factor(data[,opt[i]]))
          data[,opt[i]] <- list(as.factor(value))
        else if (is.numeric(data[,opt[i]]))
          data[,opt[i]] <- list(as.numeric(value))
        else if (is.logical(data[,opt[i]]))
          data[,opt[i]] <- list(as.logical(value))
        else
          data[,opt[i]] <- list(value)
      }
    data <- data[1:maxl,]
  }
  X <- model.matrix(delete.response(tt), data = data, contrasts =
                    if (length(object$contrasts)>0) object$contrasts
                    else NULL, xlev = object$xlevels, ...)
  row.names(X) <- 1:nrow(X)
  X <- as.matrix(X)
  class(X) <- "setx"
  if (cond) {
    X <- cbind(Y, X)
    if (!is.null(counter)) {
      X <- list(treat=X[treat==1,], control=X[treat==0,])
      class(X$treat) <- class(X$control) <- c("setx", "cond")
      class(X) <- "setx.counter"
    }
    else
      class(X) <- c("setx", "cond")
  }
  return(X)
}
