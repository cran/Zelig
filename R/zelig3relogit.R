zelig3relogit <- function(res, fcall = NULL, zcall = NULL) {

  if ("relogit2" %in% class(res)) {
    zcall$tau <- res$tau1$tau
    res$tau1$call <- as.call(zcall)
    zcall$tau <- res$tau2$tau
    res$tau2$call <- as.call(zcall)
  }

  if (is.null(zcall$robust)) {
    if (is.null(zcall$weighting))
      rob <- FALSE
    else if (zcall$weighting) {
      warning("robust is set to TRUE because weighting is used")
      rob <- TRUE
    }
    else
      rob <- FALSE
  }
  else if (is.logical(zcall$robust))
    if (!zcall$robust) {
      rob <- TRUE
      warning("robust is set to TRUE because weighting is used")
    }
  else
    rob <- zcall$robust
  if (is.list(rob)) {
    require(sandwich)
    if (!any(rob$method %in% c("vcovHAC", "kernHAC", "weave")))
      stop("such a robust option is not supported")
    else {
      if ("relogit2" %in% class(res)) {
        class(res$tau1) <-  class(res$tau2) <- c("relogit", "glm.robust")
        res$tau1$robust <- res$tau2$robust <- rob
      }
      else {
        class(res) <- c("relogit", "glm.robust")    
        res$robust <- rob
      }
    }
  }
  else if (!is.logical(rob)) 
    stop("invalid input for robust.  Choose either TRUE or a list of options.")
  else if (rob) {
    require(sandwich)
    if ("relogit2" %in% class(res)) 
      class(res$tau1) <- class(res$tau2) <- c("relogit", "glm.robust")    
    else
      class(res) <- c("relogit", "glm.robust")
  }
  return(res)
}

