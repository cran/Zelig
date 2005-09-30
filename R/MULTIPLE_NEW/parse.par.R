parse.par <- function(par, terms, eqn, shape = NULL) {
  "%w/o%" <- function(x,y) x[!x %in% y]
  if (is.null(shape)) {
    if (any(class(terms) == "list"))
      shape <- "matrix"
    else
      shape <- "vector"
  }
  if (!shape %in% c("matrix", "vector"))
    stop("not a valid 'shape' for parameters.  Choose from \"matrix\" or \"vector\".")
  if (any(class(terms) == "list")) {
    idx <- make.parameters(terms = terms, shape = "vector")
    mat <- t(make.parameters(terms = terms, shape = "matrix"))
    syst <- rownames(mat)
    if (length(syst) == 1)
      shape <- "vector"
    par.names <- unique(na.omit(c(mat)))
    ancil <- idx %w/o% par.names
    if (any(eqn %in% ancil)) {
      if (any(eqn %in% syst)) {
        stop("  eqn cannot include both systematic and ancillary \n  parameters at the same time.")
      }
      else
        ret.val <- par[idx %in% eqn]
    }
    else { ## if eqn to be returned is a systematic component
      if (length(eqn) > 1) {
        sys.idx <- sort(pmatch(eqn, syst))
        var.idx <- sort(pmatch(eqn, idx))
      }
      else {
        sys.idx <- sort(grep(eqn, syst))
        var.idx <- sort(grep(eqn, idx))
      }
      subs <- mat[sys.idx, , drop = FALSE]
      out <- matrix(0, nrow = nrow(subs), ncol = ncol(subs),
                      dimnames = dimnames(subs))
      nas <- which(is.na(subs), arr.ind = TRUE)
      not.na <- which(!is.na(subs), arr.ind = TRUE)
      const <- attr(terms, "constraints")
      if (is.logical(const)) {  ## for no constraints
        out[not.na] <- par[var.idx]
        if (shape == "matrix") 
          ret.val <- t(out)
        else {
          out[nas] <- NA
          ret.val <- na.omit(c(t(out)))
        }
      }
      else {  ## if constraints
        for (i in 1:ncol(const)) {
          idx <- idx %w/o% ancil
          const.loc <- which(subs == colnames(const)[i], arr.ind = TRUE)
          for (j in 1:nrow(const.loc)) {
            rm.idx <- apply(not.na, 1, identical, const.loc[j,])
            not.na[rm.idx,] <- NA
          }
          out[const.loc] <- par[idx == colnames(const)[i]]
        }
        not.na <- na.omit(not.na)
        out[not.na] <- par[1:nrow(not.na)]
        if (shape == "matrix") 
          ret.val <- t(out)
        else {
          out[nas] <- NA
          ret.val <- unique(na.omit(c(t(out)))) ## FIX
        }
      }
    }
  }
  ret.val
}


