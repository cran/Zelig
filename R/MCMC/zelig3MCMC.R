  zelig3MCMCdynamicEI <- zelig3MCMChierEI <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  out$coefficients <- res
  out$formula <- zcall$formula
  out$data <- zcall$data

  if (!is.null(zcall$N))
    out$N <- zcall$N
  
  out$model <- model.frame(formula=eval(out$formula),
  data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  attr(out$terms,"intercept") <- 0
  class(out) <- "MCMCZelig"

 out
}

zelig3MCMClogit <- zelig3MCMCoprobit <- zelig3MCMCpoisson <-
  zelig3MCMCmnl <- zelig3MCMCregress <-
  zelig3MCMCtobit <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  out$coefficients <- res
  out$formula <- zcall$formula
  out$data <- zcall$data

  out$model <- model.frame(formula=eval(out$formula),
  data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  class(out) <- "MCMCZelig"

 out
}

zelig3MCMCprobit <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  if (is.null(zcall$bayes.resid)) 
    zcall$bayes.resid <- FALSE

  if (zcall$bayes.resid==FALSE)
    out$coefficients <- res
  else 
    {
      p<-dim(model.matrix(eval(zcall$formula), eval(zcall$data)))[2]
      out$coefficients <- res[,1:p]
      out$bayes.residuals <- res[, -(1:p)]
    }  
  
  out$formula <- zcall$formula
  out$data <- zcall$data

  out$model <- model.frame(formula=eval(out$formula),data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  class(out) <- "MCMCZelig"

 out

  }



  zelig3MCMCfactanal <- zelig3MCMCordfactanal <- zelig3MCMCmixfactanal <- function(res, fcall=NULL, zcall=NULL) {

  out <- list()
  out$coefficients <- res
  out$formula <- zcall$formula
  out$data <- zcall$data
  
  out$model <- model.frame(formula=eval(out$formula),
  data=eval(out$data))
  out$terms <- attr(out$model, "terms")
  attr(out$terms,"intercept") <- 0
  class(out) <- "MCMCZelig"

 out
}




