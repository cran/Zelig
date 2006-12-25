plot.ci <- function(x, CI=95, qi = "ev", main = "",
                    ylab = NULL, xlab = NULL, xlim = NULL,
                    ylim = NULL, col = c("red", "blue"), ...) {
  if (class(x) != "zelig")
    stop(" plot.ci() works only for sim() output.")
  if (!(x$zelig.call$model) %in% c("ls", "logit", "probit", "exp", "gamma", "lognorm",
                                        "weibull", "normal", "poisson", "relogit",
                                        "negbin", "MCMCregress", "MCMClogit", "MCMCprobit",
                                        "MCMCpoisson", "MCMCregress"))
    stop("\n  plot.ci() is valid only for non-categorical, univariate response models.")
  var <- var1 <- list()
  idx1 <- idx <- NULL
  cip <- c((100-CI)/200, 1-(100-CI)/200)
  summarize <- function(z, cip){
    res <- NULL
    res <- cbind(res, apply(z, 2, quantile, prob=cip[1]))
    res <- cbind(res, apply(z, 2, quantile, prob=cip[2]))
    res
  }
  for (i in 1:ncol(x$x)) {
    var[[i]] <- unique(x$x[,i])
    if (length(var[[i]]) > 1)
      idx[i] <- i
    else
      idx[i] <- NA
  }
  if (!is.null(x$x1)) {
    idx1 <- NULL
    for (i in 1:ncol(x$x1)) {
      var1[[i]] <- unique(x$x1[,i])
      if (length(var1[[i]]) > 1) 
        idx1[i] <- i
      else
        idx1[i] <- NA
    }
    idx1 <- na.omit(idx1)
  }
  idx <- na.omit(idx)
  if (!is.null(idx1))
    if (!identical(idx, idx1))
      stop(" x and x1 in vary on different dimensions.")
  var <- var[[idx[1]]]
  q <- pmatch(qi, names(x$qi))
  qofi <- x$qi[[q]]
  sum.qi <- summarize(qofi, cip)
  if (!is.null(x$x1) && qi == "ev") {
    fd <- x$qi$fd
    ev1 <- fd + qofi
    sum.qi1 <- summarize(ev1, cip)
  }
  else
    sum.qi1 <- NULL
  if (is.null(ylab))
    ylab <- x$qi.name[[q]]
  if (is.null(xlab))
    xlab <- paste("Range of", colnames(x$x)[idx[1]])
  if (is.null(ylim)) {
    if (is.null(sum.qi1))
      ylim <- c(min(sum.qi), max(sum.qi))
    else
      ylim <- c(min(sum.qi, sum.qi1), max(sum.qi, sum.qi1))
  }
  if (is.null(xlim))
    xlim <- c(min(var), max(var))
  plot.default(var, type = "n", ylab = ylab, main = main, xlab = xlab, 
               xlim = xlim, ylim = ylim)
  for (i in 1:length(var)) {
    lines(c(var[i], var[i]), c(sum.qi[i,1], sum.qi[i,2]), col = col[1], ...)
    if (!is.null(x$x1) && qi == "ev")
      lines(c(var[i], var[i]), c(sum.qi1[i,1], sum.qi1[i,2]), col = col[2], ...)
  }
}










