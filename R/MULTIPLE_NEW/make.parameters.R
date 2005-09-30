make.parameters <- function(terms, shape = "vector", ancillary = TRUE) {
  if (!shape %in% c("matrix", "vector"))
    stop("not a valid 'shape' for parameters.  Choose from \"matrix\" or \"vector\".")
  ints <- attr(terms, "intercept")
  eqns <- names(ints)
  labs <- attr(terms, "term.labels")
  const <- attr(terms, "constraints")
  const[const == 0] <- NA
  for (i in 1:length(eqns)) {
    if (ints[[i]] == 1)
      labs[[i]] <- c("(Intercept)", labs[[i]])
  }
  "%w/o%" <- function(x,y) x[!x %in% y]
  fixed <- which(labs == "(Intercept)")
  syst <- 1:length(eqns) %w/o% fixed
  vars <- unique(unlist(labs))
  pars <- matrix(NA, ncol = length(syst), nrow = length(vars))
  colnames(pars) <- eqns[syst]
  rownames(pars) <- vars
  for (i in syst) {
    idx <- which(!is.na(match(vars, labs[[i]])))
    pars[idx,i] <- paste(labs[[i]], eqns[i], sep = ":")
  }
  if (!is.logical(const)) {
    for (i in 1:ncol(const)) {
      cidx <- which(!is.na(const[,i]))
      ridx <- match(const[cidx, i], rownames(pars))
      pars[cbind(ridx, cidx)] <- colnames(const)[i]
    }
  }
  if (shape == "matrix")
    out <- pars
  if (shape == "vector") {
    out <- unique(na.omit(c(t(pars))))
    if (ancillary) 
      out <- c(out, eqns[fixed])
  }
  out
}
