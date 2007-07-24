MIsimulation <- function (object, num = c(1000, 100), prev = NULL, 
                          bootstrap = FALSE, bootfn = NULL, ...)  {
  M <- length(object)
  simpar <- simqi <- res <- list()
  if (is.null(prev)) {
    numM <- round(num/M)
    if (!bootstrap) {
      for (i in 1:M)
        simpar[[i]] <- param(object[[i]], num = numM, bootstrap =
                             bootstrap)
    }
    else {
      tt <- terms(object[[1]])
      if (missing(bootfn)) 
        bootfn <- bootfn.default
      for (i in 1:M) {
        dta <- eval(object[[i]]$data, sys.parent())
        res <- boot(dta, bootfn, R = num, object = object[[i]], ...)
        colnames(res$t) <- names(res$t0)
        simpar[[i]] <- res$t
      }
    }
    params <- as.matrix(simpar[[1]])
    for (j in 2:M)
      params <- rbind(params, as.matrix(simpar[[j]]))
  }
  else {
    if (bootstrap) 
      stop("Error: Choosing 'bootstrap = TRUE' generates new parameters.  \nIf you wish to use previously generated parameters, \nplease specify only 'prev'.")
    else params <- prev
  }
  params
}

