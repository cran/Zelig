define.data <- function(formula, data, x.name = "x",
                        y.name = "y", mf.name = "mf",
                        xlev.name = "xlev", lev.name = "lev") {
  mc <- match.call()
  mc$x.name <- mc$y.name <- mc$t.name <- mc$xlev.name <- mc$l.name <- NULL

  mc[[1]] <- as.name("model.frame")
  mf <- eval.parent(mc)
  assign(mf.name, mf, envir = parent.frame())
  
  Terms <- attr(mf, "terms")

  x <- model.matrix(Terms, mf)
  assign(x.name, x, envir = parent.frame())

  yvar <- attr(Terms, "response")
  xvars <- as.character(attr(Terms, "response"))[-1]
  xlev <- if (length(xvars) > 0) {
    xlev <- lapply(mf[xvars], levels)
    xlev[!sapply(xlev, is.null)]
  }
  assign(xlev.name, xlev, envir = parent.frame())

  y <- model.response(mf)
  assign(y.name, y, envir = parent.frame())

  lev <- levels(y)
  assign(lev.name, lev, envir = parent.frame())

  attr(Terms, "items") <- list(x = x.name, y = y.name,
                               mf = mf.name, xlev =
                               xlev.name, lev = lev.name)
  Terms
}
