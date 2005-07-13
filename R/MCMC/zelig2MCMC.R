zelig2MCMChierEI <- function(formula, model, data, M, ...) {
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
  
  if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else
      {
        if (is.null(mf$mcmc))  mcmc <- 50000
           else mcmc <- mf$mcmc
        if (is.null(mf$burnin)) burnin <- 5000
           else burnin <- mf$burnin
        mf$verbose <- round((mcmc+burnin)/10)
      }
  
  mf$model <- mf$M <- NULL
  temp <- mcmcei(formula=formula, data=data, covar=covar)

  if ((any(temp<0)) || ((any(temp<1) && !any(temp==0) ) && any(temp>1)))
    stop("data need to be either counts or proportions.\n") 
  if (is.null(mf$N))
    {
       if (all(temp>=0))  #N is not needed
        {
          mf$r0 <- temp$r0
          mf$r1 <- temp$r1
          mf$c0 <- temp$c0
          mf$c1 <- temp$c1
        }
      else stop("Needs total counts for inputs as porportion.\n")
    }
  else if (((length(mf$N)!= nrow(data)) && (length(mf$N)!=1)) || (any(mf$N<1)))
    stop("N needs to have same length as the observations and be postive numbers\n.")
  else if ((all(temp<1)) && (all(mf$N>1)))
      {
        mf$r0 <- round(temp$r0*mf$N)
        mf$r1 <- mf$N-mf$r0
        mf$c0 <- round(temp$c0*mf$N)
        mf$c1 <- mf$N-mf$c0

      }
  
  mf[[1]] <- MCMCpack::MCMChierEI
  as.call(mf)
}


zelig2MCMClogit <-  function(formula, model, data, M, ...) {    
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)

  if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else
      {
        if (is.null(mf$mcmc))  mcmc <- 10000
           else mcmc <- mf$mcmc
        if (is.null(mf$burnin)) burnin <- 1000
           else burnin <- mf$burnin
        mf$verbose <- round((mcmc+burnin)/10)
      }
  
  mf$model <- mf$M <- NULL
 
  mf[[1]] <- MCMCpack::MCMClogit
  as.call(mf)
}

zelig2MCMCprobit <-  function(formula, model, data, M, ...) {    
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)

  if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else
      {
        if (is.null(mf$mcmc))  mcmc <- 10000
           else mcmc <- mf$mcmc
        if (is.null(mf$burnin)) burnin <- 1000
           else burnin <- mf$burnin
        mf$verbose <- round((mcmc+burnin)/10)
      }

  mf$model <- mf$M <- NULL

  mf[[1]] <- MCMCpack::MCMCprobit
  as.call(mf)
}

zelig2MCMCregress <-  function(formula, model, data, M, ...) {    
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
 if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else
      {
        if (is.null(mf$mcmc))  mcmc <- 10000
           else mcmc <- mf$mcmc
        if (is.null(mf$burnin)) burnin <- 1000
           else burnin <- mf$burnin
        mf$verbose <- round((mcmc+burnin)/10)
      }

  mf[[1]] <- MCMCpack::MCMCregress
  as.call(mf)
}

zelig2MCMCpoisson <-  function(formula, model, data, M, ...) {    
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
 if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else
      {
        if (is.null(mf$mcmc))  mcmc <- 10000
           else mcmc <- mf$mcmc
        if (is.null(mf$burnin)) burnin <- 1000
           else burnin <- mf$burnin
        mf$verbose <- round((mcmc+burnin)/10)
      }
  mf[[1]] <- MCMCpack::MCMCpoisson
  as.call(mf)
}

zelig2MCMCoprobit <-  function(formula, model, data, M, ...) {    
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
 if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else mf$verbose <- 500 
  mf[[1]] <- MCMCpack::MCMCoprobit
  as.call(mf)
}




zelig2MCMCmnl <-  function(formula, model, data, M, ...) {    
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
  mf$model <- mf$M <- NULL
 if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else mf$verbose <- 500
  mf[[1]] <- MCMCpack::MCMCmnl
  as.call(mf)
}




zelig2MCMCtobit <-  function(formula, model, data, M, ...) {    
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
 if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else mf$verbose <- 500
  mf$model <- mf$M <- NULL
  mf[[1]] <- MCMCpack::MCMCtobit
  as.call(mf)
}

zelig2MCMCdynamicEI <- function(formula, model, data, M, ...) {
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
  if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else mf$verbose <- 500 
  mf$model <- mf$M <- NULL
  temp <- mcmcei(formula=formula, data=data, covar=covar)
  mf$r0 <- temp$r0
  mf$r1 <- temp$r1
  mf$c0 <- temp$c0
  mf$c1 <- temp$c1
 # mf$formula <- mf$data <- NULL
  mf[[1]] <- MCMCpack::MCMCdynamicEI
  as.call(mf)
}




zelig2MCMCfactanal <- function(formula, model, data, M, ...) {
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
  if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else mf$verbose <- 500 
  mf$model <- mf$M <- NULL
  mf$x <- as.matrix(model.frame(formula, data=data, na.action=NULL))
  mf[[1]] <- MCMCpack::MCMCfactanal
  as.call(mf)
}

zelig2MCMCordfactanal <- function(formula, model, data, M, ...) {
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
  if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else mf$verbose <- 500 
  mf$model <- mf$M <- NULL
  mf$x <- as.matrix(model.frame(formula, data=data, na.action=NULL))
  mf[[1]] <- MCMCpack::MCMCordfactanal
  as.call(mf)
}

zelig2MCMCmixfactanal <- function(formula, model, data, M, ...) {
  require(MCMCpack)
  mf <- match.call(expand.dots = TRUE)
  if (is.null(mf$verbose) || !mf$verbose) mf$verbose <- 0
    else mf$verbose <- 500 
  mf$model <- mf$M <- NULL
  var <- model.response(model.frame(formula, data=data,
  na.action=NULL))
  varnames <- colnames(var)
  mf$x <- as.formula(paste("~", paste(varnames, collapse="+")))
  mf[[1]] <- MCMCpack::MCMCmixfactanal
  as.call(mf)
}

